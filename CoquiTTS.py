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
except Exception as e:
    print(f"An thread error occurred: {e}")
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
async def generate_audio_stream():
    buffer = ""
    for chunk in response:
        if 'choices' in chunk:
            delta = chunk['choices'][0]['delta']
            if 'content' in delta:
                buffer += delta['content']
                # Yield the text chunks as they come
                yield delta['content']
    
    # After receiving all text, convert to audio
    try:
        from gtts import gTTS

        tts = gTTS(text=buffer, lang='en')
        mp3_fp = io.BytesIO()
        tts.write_to_fp(mp3_fp)
        mp3_fp.seek(0)

        # Yield the audio content
        audio_content = mp3_fp.read()
        yield audio_content
    except Exception as e:
        print(f"TTS error: {e}")
        raise HTTPException(status_code=500, detail="TTS conversion error.")
    return StreamingResponse(generate_audio_stream(), media_type="audio/mpeg", headers={"Content-Disposition": "attachment; filename=audio.mp3"})
