gore-and-ash-network
====================

The module provides facilities for basic networking for [Gore&Ash](https://github.com/Teaspot-Studio/gore-and-ash) engine.

The module depends on [gore-and-ash-logging](https://github.com/Teaspot-Studio/gore-and-ash-logging) module.

Installing
==========

Add following to your `stack.yml` to `packages` section:
```yaml
- location:
    git: https://github.com/Teaspot-Studio/gore-and-ash-network.git
    commit: <PLACE HERE FULL HASH OF LAST COMMIT> 
```

When defining you application stack, add `NetworkT`:
``` haskell
type AppStack = ModuleStack [LoggingT, NetworkT, ... other modules ... ] IO
```

And derive `NetworkMonad` for your resulting `AppMonad`:
``` haskell
newtype AppMonad a = AppMonad (AppStack a)
  deriving (Functor, Applicative, Monad, MonadFix, MonadIO, MonadThrow, MonadCatch LoggingMonad, NetworkMonad)
```