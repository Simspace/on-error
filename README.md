# on-error: clearly-delineated error-handling

[![Build Status](https://travis-ci.com/simspace/on-error.svg?branch=master)](https://travis-ci.com/simspace/on-error)

Error-handling code is messy, but if we want to provide good error messages, we should still take the time to do it
right. `on-error` provides a naming convention which clearly distinguishes error-handling code from the rest of the
code, thereby allowing developers to only pay attention to the error-handling code when they want to do so.

The naming convention is as follows:

1. `on[Condition]ThrowError` functions detect errors
2. `[transform]Error` functions transform errors
3. `onError[Action]` functions handle errors

Furthermore, those functions are designed to be composed using `(.)` into a clearly-delineated block of error-handling
code. Here is an example:

    countDogs :: Response Int
    countDogs = onErrorCatch sendErrorStatus
              . onNothingThrowError 500
           =<<$ liftIO
              $ fetchCount "http://example.com/dogs/count"

    fetchCount :: Text -> IO (Maybe Int)
    sendErrorStatus :: Int -> Response a

Everything before the `(=<<$)` (which is just a low-precedence version of `(=<<)`) is error-handling code, and
everything after it is normal code. Once our brains have learned to recognize those error-handling blocks as such, we
can take a brief look at the above definition and quickly home in to the important part: `countDogs` calls `fetchCount`
at a specific URL, while the rest of the code embeds this `IO` computation into a `Response` computation, and deals with
the error cases somehow. This is often a good enough level of understanding, but if later on we do need to understand
the error-handling code, we can take a closer look and determine that the `Nothing` case is handled by calling
`sendErrorStatus 500`.

Without `on-error`, the implementation of `countDogs` might look something like this:

    countDogs' :: Response Int
    countDogs' = do
      r <- liftIO $ fetchCount "http://example.com/dogs/count"
      case r of
        Nothing -> sendErrorStatus 500
        Just count -> pure count

Or like this:

    countDogs'' :: Response Int
    countDogs'' = join
                . fmap (maybe (sendErrorStatus 500) pure)
                . liftIO
                $ fetchCount "http://example.com/dogs/count"

`countDogs'` is quite clear, but you only realize that the second part of the function only performs error-handling
after you have already read and understood that part. `countDogs''` uses a different style in which the core
`fetchCount` computation is gradually transformed into a `Response` computation, but again, it's unclear whether some of
those transformations affect the happy path or just the error cases until after you've read and understood those
transformations.


## Detecting errors

Functions which can fail do so in one of three ways:

1. By returning a special value such as `Nothing` or `Either`.
2. By signaling failure using some monadic effect, such `ExceptT.throwE`, `MonadError.throwError`, or `MonadFail.fail`.
3. By throwing an exception.

In each case, we want to detect the error condition and to convert it to a value of some type `e` representing the
errors which we know can happen within the current code. If you only plan to log the error or to display it to the user,
`Text` is a good enough representation, but if you plan to handle some of those errors later on, `SomeException` and
`Text` are terrible representations because they don't give your callers any information about the set of error cases
they might want to handle. If you want to do error-_handling_, not just error-displaying, a sum type would be a better
choice for `e`. See [Handling errors](#handling-errors) for some concrete suggestions.

In any case, here's how to obtain an `e` in all three cases.

1.  For `Nothing`, use `onNothingThrowError` with a value of type `e` to be thrown if the value is `Nothing`.

    For `Left x`, use `onLeftThrowError`. It uses `x` as the error, which you can then convert to an `e` using
    `mapError`. For other, less common values, define a custom `on[Condition]ThrowError` function in order to avoid
    polluting your non-error-handling code with error-handling concerns such as converting to `Maybe` or `Either`.
2.  For `ExceptT.throwE` and `MonadError.throwError`, there is nothing to do, because `Control.Monad.Trans.OnError`
    already uses `ExceptT` to propagate the error upwards. The `Control.Monad.OnError` API is slightly different in that
    regard, see the [Propagating and transforming errors](#propagating-and-transforming-errors) section for details.

    For `MonadFail.fail`, the behaviour depends on the monad. Due to a design wart, calling `fail` often throws an
    exception, even for error-tracking monads such as `Either` and `ExceptT`.
3.  For exceptions, try to catch them as close to their source as possible and to rethrow them as errors using one of
    the two other methods. Be careful to only catch the exceptions you know about; blindly catching all exceptions and
    propagating them up as a `SomeException` or a `Text` will not improve the quality of your error handling, it will
    only decrease it since your callers won't know what to handle and it will be much more difficult to make sure the
    generated error messages are valid english sentences. It is better to let unknown exceptions propagate upwards as
    exceptions, not errors, and to handle exceptions generically at the top-level of your program. Because of
    asynchronous exceptions, all the code you write has to be exception-safe anyway.


## Propagating and transforming errors

Once an error is detected, the computation stops and the error gets propagated up the stack until it gets handled.
`on-error`'s two modules provide two alternate ways of doing that:

1.  `Control.Monad.Trans.OnError` is based on `transformers`, in which case `ExceptT` should be the outermost
    transformer.
2.  `Control.Monad.OnError` is based on `mtl`, in which case the computation should be polymorphic in `m` and have a
    `MonadError e m` constaint.

Learning `Control.Monad.Trans.OnError` first is recommended, because its type signatures are more intuitive. The type
signatures of the `Control.Monad.OnError` module are a bit misleading because they often ask for an `ExceptT`
computation when a computation which is polymorphic in `m` would be a better choice. For example, the type of
`Control.Monad.OnError.mapError` is

    mapError :: MonadError e' m => (e -> e') -> ExceptT e m a -> m a

And so we might be tempted to give it an `ExceptT e m a` computation:

    countHumans :: forall m. (MonadError Text m, MonadIO m) => m Int
    countHumans = mapError ("while counting humans: " <>) body
      where
        body :: ExceptT Text m Int
        body = do
          liftIO $ putStrLn "counting humans..."
          throwE "humans are not pets"

This typechecks, but using such a concrete monad stack doesn't fit well with the `mtl` style for which
`Control.Monad.OnError` is designed. It would be better to use a polymorphic monad stack:

    countHumans :: forall m. (MonadError Text m, MonadIO m) => m Int
    countHumans = mapError ("while counting humans: " <>) body
      where
        body :: forall n. (MonadError Text n, MonadIO n) => n Int
        body = do
          liftIO $ putStrLn "counting humans..."
          throwError "humans are not pets"

This typechecks as well, since `n` automatically gets specialized to `ExceptT Text m`.

The reason `mapError` specializes the `n` of its input computation in this way is that changing the type of the error
being propagated is not an effectful action in any monad, it is instead a translation from an `ExceptT e` computation to
an `ExceptT e'` computation. By specializing `n a` to `ExceptT e m a`, we can strip off the `ExceptT e` layer to obtain
an `m (Either e a)`, at which point we can convert the `e` to an `e'` and rethrow it using `m`'s `MonadError` instance.
This means that `m` will itself be instantiated to a monad stack containing an `ExceptT e'` at some point, and so if the
`n a` computation was using a concrete monad stack, it would look something like `ExceptT e (ExceptT e' IO) a`. This is
a pretty unusual and unintuitive monad stack, which is another reason to prefer writing it as a computation which is
polymorphic in `n`.


## Handling errors

While you can use the `onError[Action]` functions to handle the errors in whichever way you please, here are some
concrete recommendations.

For a function which looks up a key in a `Map`, it makes sense to return a `Maybe` to denote the fact that the key was
not found. The `Nothing` case isn't necessarily an error case; perhaps the caller wants to insert a new value at that
key, and the `Nothing` case is actually the success case because there isn't an existing, conflicting value at that key.

So when we are very close to the source of the "error", it's not yet clear whether that error is problematic or not,
because we do not yet have enough context. So we propagate the information upwards, in the hope that the caller has more
context.

If we are manipulating a graph represented as a `Map` from node to neighbours, we know that our invariant is that all
the neighbour nodes must be present in the `Map`. So if we attempt to perform a lookup and we receive a `Nothing`, we
know that we have a bug somewhere which accidentally breaks the invariant. There is nothing the caller can do about
this, the only solution is to abort and to inform the programmer that a bug needs to be fixed. Since there is nothing
the caller can do, it is not useful to tell it that this particular error case could happen, and so for bugs, I don't
recommend propagating the error up using `on-error`, instead I recommend failing with an exception, for example using
`error "invariant violated: neighbour not in Map"`.

Keeping track of such invariants in order to know when to convert unlikely errors into exceptions which will hopefully
never be thrown is an important part of error-handling, because it allows you to reduce the number of error cases you
are propagating up. Otherwise, as we go up the stack, functions have more and more sub-calls beneath them, and so more
and more error cases would accumulate, and handling all those cases would become unmanageable. I recommend trying to
keep the number of error cases small at all levels.

At the top-level, the caller is the user. For them, a sum type describing all the possible error cases is less useful;
what they need is a clear error message. So once we have enough context to know that an error cannot be handled by the
code and will have to be displayed to the user, I recommend converting the value representing the error to `Text`, and to
propagate that error message upwards. With judicious uses of `annotateError`, this error message can be annotated with
some contextual information clarifying where the error has occurred.

For example, if the user provides a pair of keys so we can perform some lookup in a nested `Map` of `Map`s, we'll have
to tell the user which key wasn't found and in which `Map`:

    lookupM :: MonadError Text m
            => Int -> Map Int a -> m a
    lookupM k = onNothingThrowError ("key " <> showt k <> " not found")
              . Map.lookup k

    nestedLookupM :: MonadError Text m
                  => (Int, Int) -> Map Int (Map Int a) -> m a
    nestedLookupM (k1, k2) mm = do
      m <- annotateError "outer map" $ lookupM k1 mm
      a <- annotateError "inner map" $ lookupM k2 m
      pure a

    -- |
    -- >>> nestedLookupM (3,4) nestedMap :: Either Text Int
    -- Left "outer map: key 3 not found"
    -- >>> nestedLookupM (1,4) nestedMap :: Either Text Int
    -- Left "inner map: key 4 not found"
    nestedMap :: Map Int (Map Int Int)
    nestedMap = Map.fromList [(1, Map.fromList [(2, 42)])]
