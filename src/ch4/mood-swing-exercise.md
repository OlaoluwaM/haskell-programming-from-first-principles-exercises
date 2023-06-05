# Mood Swing Exercise

```haskell
module MoodSwing where

  data Mood = Blah | Woot deriving Show

  changeMood :: Mood -> Mood
  changeMood mood = Woot
  changeMood _ = Blah
```

1. `Mood`
2. `Blah | Woot`
3. Data constructors are for the term level and cannot be used at the type level
4.
