module Control.Monad.Extra where

-- | A version of '=<<' with the low precedence of '$'. Useful for delineating an error-handling block from the monadic
-- computation which produces the error, especially if that compuation contains '$'s.
--
-- > countDogs :: Response Int
-- > countDogs = onErrorCatch sendErrorStatus
-- >           . onNothingThrowError 500
-- >        =<<$ liftIO
-- >           $ fetchCount "http://example.com/dogs/count"
(=<<$) :: Monad m => (a -> m b) -> m a -> m b
(=<<$) = (=<<)
infixr 0 =<<$
