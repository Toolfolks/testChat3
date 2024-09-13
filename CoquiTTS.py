import openai
import threading
import queue
import os
import sys
import sounddevice as sd
import numpy as np
from TTS.api import TTS

# Ensure UTF-8 encoding for stdout (for proper display of characters)
if sys.version_info >= (3, 7):
    sys.stdout.reconfigure(encoding='utf-8')

# Retrieve the OpenAI API key from environment variables
OPENAI_API_KEY = os.getenv('OPENAI_API_KEY')


openai.api_key = OPENAI_API_KEY

# Initialize the Coqui TTS model
print("Loading Coqui TTS model...")
tts = TTS(model_name="tts_models/en/ljspeech/tacotron2-DDC", progress_bar=False, gpu=False)
print("Coqui TTS model loaded.")

# Thread-safe queue for communication between streaming and TTS threads
text_queue = queue.Queue()

def stream_openai_responses(prompt, model="gpt-4"):
    """
    Streams responses from OpenAI's ChatCompletion API and enqueues text chunks.

    Args:
        prompt (str): The input prompt for the assistant.
        model (str): The OpenAI model to use.
    """
    try:
        response = openai.ChatCompletion.create(
            model=model,
            messages=[{"role": "user", "content": prompt}],
            temperature=0.7,    # Adjust for creativity
            max_tokens=500,     # Adjust based on your needs
            stream=True         # Enable streaming
        )

        print("\nAssistant's Reply:")
        for chunk in response:
            if 'choices' in chunk and len(chunk['choices']) > 0:
                delta = chunk['choices'][0]['delta']
                if 'content' in delta:
                    text = delta['content']
                    print(text, end='', flush=True)
                    text_queue.put(text)
        print()  # For newline after completion
    except Exception as e:
        print(f"\nError communicating with OpenAI: {e}")
        text_queue.put(None)  # Signal termination

def tts_worker():
    """
    Worker function that dequeues text chunks, synthesizes speech, and plays audio.
    """
    while True:
        text = text_queue.get()
        if text is None:
            break  # Termination signal received
        if text.strip() == "":
            continue  # Skip empty strings
        # Synthesize speech using Coqui TTS
        try:
            # Synthesize speech; returns a numpy array of audio samples
            wav = tts.tts(text, speaker_name=None, language_name=None, sample_rate=None)
            # Normalize audio to prevent clipping
            wav = wav / np.max(np.abs(wav)) if np.max(np.abs(wav)) != 0 else wav
            # Play audio using sounddevice
            sd.play(wav, tts.synthesizer.output_sample_rate)
            sd.wait()  # Wait until audio is done playing
        except Exception as e:
            print(f"\nError in TTS synthesis or playback: {e}")

def main():
    """
    Main function to execute the workflow:
    1. Get user input.
    2. Start streaming and TTS threads.
    3. Wait for completion.
    """
    print("Welcome to the OpenAI ChatGPT Real-Time TTS Assistant using Coqui TTS!")
    prompt = input("Enter your prompt for the assistant: ")

    if not prompt.strip():
        print("Empty prompt. Exiting.")
        return

    # Start the TTS thread
    tts_thread = threading.Thread(target=tts_worker, daemon=True)
    tts_thread.start()

    print("\nCommunicating with OpenAI ChatGPT...\n")

    # Start streaming OpenAI responses
    stream_openai_responses(prompt)

    # After streaming is done, send termination signal to TTS thread
    text_queue.put(None)

    # Wait for the TTS thread to finish
    tts_thread.join()

    print("\nConversation ended.")

if __name__ == "__main__":
    main()
