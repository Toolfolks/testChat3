# CoquiTTS.py

import os
import io
import logging
from fastapi import FastAPI, HTTPException
from fastapi.middleware.cors import CORSMiddleware
from pydantic import BaseModel
from fastapi.responses import StreamingResponse
import openai
import boto3
from botocore.exceptions import BotoCoreError, ClientError
import asyncio


# Initialize logging
logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)

# Initialize OpenAI API key
openai.api_key = os.getenv('OPENAI_API_KEY')  # Ensure your API key is set

# Initialize AWS Polly client
polly_client = boto3.client('polly')

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

@app.post("/chatinput")
async def stream_audio(request: TextRequest):
    try:
        # Send user message to OpenAI ChatCompletion API with streaming
        response = openai.ChatCompletion.create(
            model="gpt-4",
            messages=[
                {"role": "system", "content": "You are ChatGPT."},
                {"role": "user", "content": request.text},
            ],
            stream=True  # Enable streaming
        )
    except Exception as e:
        logger.error(f"OpenAI API error: {e}")
        raise HTTPException(status_code=500, detail="OpenAI API error.")

    async def synthesize_speech(text_chunk: str) -> bytes:
        """
        Synthesize speech using AWS Polly for the given text chunk.
        Runs the synchronous boto3 call in a separate thread to avoid blocking.
        """
        try:
            polly_response = polly_client.synthesize_speech(
                Text=text_chunk,
                OutputFormat='mp3',
                VoiceId='Joanna'  # Choose an appropriate voice
            )
            audio_stream = polly_response['AudioStream'].read()
            return audio_stream
        except (BotoCoreError, ClientError) as polly_error:
            logger.error(f"AWS Polly error: {polly_error}")
            raise HTTPException(status_code=500, detail="TTS conversion error.")

    async def generate_audio_stream():
        try:
            async for chunk in response:
                if 'choices' in chunk:
                    delta = chunk['choices'][0]['delta']
                    if 'content' in delta:
                        text_chunk = delta['content']
                        logger.debug(f"Received text chunk: {text_chunk}")

                        # Convert text chunk to speech using AWS Polly in a separate thread
                        audio_bytes = await asyncio.to_thread(synthesize_speech, text_chunk)
                        yield audio_bytes
        except Exception as e:
            logger.error(f"Error while streaming from OpenAI: {e}")
            raise HTTPException(status_code=500, detail="Error streaming from OpenAI.")

    return StreamingResponse(
        generate_audio_stream(),
        media_type="audio/mpeg",
        headers={"Content-Disposition": "attachment; filename=audio.mp3"}
    )
