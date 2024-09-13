# CoquiTTS.py

import os
import io
import logging
from fastapi import FastAPI, HTTPException
from fastapi.middleware.cors import CORSMiddleware
from pydantic import BaseModel
from fastapi.responses import StreamingResponse
from openai import OpenAI
import openai
import asyncio
from gtts import gTTS

# Initialize logging
logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)

# Initialize the OpenAI client
openai.api_key = os.getenv('OPENAI_API_KEY')  # Ensure your API key is set

# Initialize FastAPI
app = FastAPI()

# Configure CORS to allow all origins (for development/testing purposes)
app.add_middleware(
    CORSMiddleware,
    allow_origins=["*"],  # Allow all origins
    allow_credentials=True,
    allow_methods=["*"],  # Allow all HTTP methods including OPTIONS
    allow_headers=["*"],  # Allow all headers
)

# Define the input model
class TextRequest(BaseModel):
    text: str

# Initialize OpenAI API client
client = OpenAI()

# Define your assistant and thread setup here (assuming it's necessary)
existing_assistant_id = "asst_KwbkEYapMSuJDNHO6qGtyazI"

# Create a thread (if required)
try:
    my_thread = client.beta.threads.create()
    logger.info(f"Thread created with ID: {my_thread.id}")
except Exception as e:
    logger.error(f"An thread error occurred: {e}")
    my_thread = None  # Handle accordingly

def add_message_to_thread(thread_id, role, content):
    """Adds a message to the thread and returns the message object."""
    message = client.beta.threads.messages.create(
        thread_id=thread_id,
        role=role,
        content=content,
    )
    return message

@app.post("/chatinput")
async def stream_audio(request: TextRequest):
    if not my_thread:
        logger.error("Thread not initialized.")
        raise HTTPException(status_code=500, detail="Thread not initialized.")

    # Add user message to the thread
    try:
        add_message_to_thread(my_thread.id, "user", request.text)
        logger.info(f"User message added to thread {my_thread.id}")
    except Exception as e:
        logger.error(f"Failed to add message to thread: {e}")
        raise HTTPException(status_code=500, detail="Failed to add message to thread.")

    # Prepare the OpenAI API call with streaming
    try:
        response = openai.ChatCompletion.create(
            model="gpt-4",  # Use the appropriate model
            messages=[
                {"role": "system", "content": "You are ChatGPT."},
                {"role": "user", "content": request.text},
            ],
            stream=True  # Enable streaming
        )
        logger.info("OpenAI ChatCompletion streaming started.")
    except Exception as e:
        logger.error(f"OpenAI API error: {e}")
        raise HTTPException(status_code=500, detail="OpenAI API error.")

    async def generate_audio_stream():
        buffer = ""
        try:
            for chunk in response:
                if 'choices' in chunk:
                    delta = chunk['choices'][0]['delta']
                    if 'content' in delta:
                        buffer += delta['content']
                        # Yield the text chunks as they come
                        logger.debug(f"Yielding text chunk: {delta['content']}")
                        yield delta['content']
        except Exception as e:
            logger.error(f"Error while streaming from OpenAI: {e}")
            raise HTTPException(status_code=500, detail="Error streaming from OpenAI.")

        # After receiving all text, convert to audio
        try:
            logger.info("Converting accumulated text to audio using gTTS.")
            tts = gTTS(text=buffer, lang='en')
            mp3_fp = io.BytesIO()
            tts.write_to_fp(mp3_fp)
            mp3_fp.seek(0)

            audio_content = mp3_fp.read()
            logger.info("Audio conversion complete. Yielding audio content.")
            yield audio_content  # Use 'yield' instead of 'yield from'
        except Exception as e:
            logger.error(f"TTS error: {e}")
            raise HTTPException(status_code=500, detail="TTS conversion error.")

    return StreamingResponse(
        generate_audio_stream(),
        media_type="audio/mpeg",
        headers={"Content-Disposition": "attachment; filename=audio.mp3"}
    )
