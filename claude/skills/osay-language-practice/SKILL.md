---
name: osay-language-practice
description: Generates natural-sounding sentences for language practice using the osay TTS tool. Use when user wants to practice speaking, reading, or listening with everyday sentences, pronunciation drills, or conversational phrases. Triggers on "practice sentences", "pronunciation", "listening practice", "speak this", "say this", or TTS generation requests.
---

# Language Practice with osay

Generate everyday, lifelike sentences with natural pronunciation for speaking, reading, and listening practice.

## Quick Reference

```bash
# Basic sentence
osay "The weather is perfect for a walk today."

# With specific voice (coral is warm and natural)
osay -v coral "I'd like a cup of coffee, please."

# Save for repeated practice
osay -o practice.mp3 "Could you repeat that more slowly?"

# Play previous audio
osay -p
```

## Voices for Practice

| Voice | Best For |
|-------|----------|
| coral | Warm, conversational dialogues |
| nova | Clear, standard pronunciation |
| onyx | Deeper, formal contexts |
| shimmer | Expressive, emotional content |
| fable | Storytelling, narrative text |

## Speech Instructions

Control tone and style for realistic practice:

```bash
# Natural conversational speed
osay --instructions "Speak naturally at a moderate pace, like talking to a friend"

# Slow and clear for beginners
osay --instructions "Speak slowly and clearly, pausing between phrases"

# Casual everyday speech
osay --instructions "Speak casually and relaxed, like everyday conversation"

# Neutral (no default cheerful tone)
osay --no-instructions "This is a factual statement."
```

## Example Sentences

**English**: See [examples/english/SENTENCES.md](examples/english/SENTENCES.md) for ready-to-use sentences organized by topic and difficulty.

## Practice Sentence Generation

When generating sentences for practice, follow these guidelines:

### Categories

1. **Daily Routines**: Morning activities, commuting, meals, work
2. **Social Interactions**: Greetings, small talk, requests, opinions
3. **Practical Situations**: Shopping, directions, appointments, phone calls
4. **Emotions & Reactions**: Expressing feelings, responding to news

### Difficulty Levels

- **Beginner**: Short sentences (5-8 words), common vocabulary, simple grammar
- **Intermediate**: Compound sentences, idioms, varied tenses
- **Advanced**: Complex structures, nuanced expressions, colloquialisms

### Example Workflow

Generate 5 sentences, save each for repeated practice:

```bash
# Generate themed sentences
osay -v coral -o ~/practice/greeting_01.mp3 "Hi, how have you been?"
osay -v coral -o ~/practice/greeting_02.mp3 "It's great to see you again!"
osay -v coral -o ~/practice/greeting_03.mp3 "What have you been up to lately?"
```

## Pronunciation Focus

For specific sounds or patterns:

```bash
# Emphasize linking sounds
osay --instructions "Speak naturally, connecting words smoothly" \
  "Would you like a cup of tea?"

# Clear enunciation
osay --instructions "Speak clearly, emphasizing each syllable" \
  "particularly, comfortable, vegetable"

# Stress patterns
osay --instructions "Natural stress on key words" \
  "I didn't say HE stole the money."
```

## Listening Comprehension

Create varied audio for listening practice:

```bash
# Fast native speed
osay --instructions "Speak at natural native speed" \
  "I'm gonna grab a coffee real quick, you want anything?"

# Reduced forms
osay --instructions "Use natural contractions and reductions" \
  "What do you want to do? -> Whaddya wanna do?"
```

## Cache for Review

Audio is cached automatically (max 10 files in `~/.osay/audios/`):

```bash
# List all cached audio
osay --list-cached

# Replay most recent
osay -p

# Select from cache with fzf
osay --play-cached
```

## Batch Practice Session

Create a practice set from a text file:

```bash
# sentences.txt contains one sentence per line
while IFS= read -r sentence; do
  osay -v coral "$sentence"
  sleep 1
done < sentences.txt
```

## Output Formats

- **mp3**: Default, good for all purposes
- **wav**: Lossless, best for detailed listening
- **opus**: Efficient for storing many files
