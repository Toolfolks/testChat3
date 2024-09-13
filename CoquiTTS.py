import os
import io
import logging
from fastapi import FastAPI, HTTPException
from fastapi.middleware.cors import CORSMiddleware
from pydantic import BaseModel
from fastapi.responses import StreamingResponse
from gtts import gTTS
import openai

# Set your OpenAI API key
openai.api_key = os.getenv('OPENAI_API_KEY')

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

# Stream response generator using OpenAI's streaming feature
async def stream_openai_response(prompt: str):
    response = openai.ChatCompletion.create(
        model="gpt-4",  # Use the chat model, change model if needed
        messages=[{"role": "user", "content": prompt}],
        max_tokens=1000,
        stream=True  # Enable streaming
    )

    # Streaming responses from OpenAI
    for chunk in response:
        if "choices" in chunk:
            chunk_message = chunk['choices'][0].get('delta', {}).get('content', '')
            if chunk_message.strip():
                yield chunk_message

@app.post("/chatinput")
async def stream_audio(request: TextRequest):
    try:
        # Stream OpenAI response text
        async def stream_speech():
            async for chunk in stream_openai_response(request.text):
                try:
                    # Convert text to speech using gTTS
                    tts = gTTS(text=chunk, lang='en')

                    # Create a BytesIO stream to hold the MP3 data
                    mp3_fp = io.BytesIO()
                    tts.write_to_fp(mp3_fp)
                    mp3_fp.seek(0)

                    # Stream the MP3 file
                    yield mp3_fp.read()
                except Exception as e:
                    print(f"An error occurred while generating speech: {e}")
                    raise HTTPException(status_code=500, detail="Internal Server Error")

        # Stream the MP3 file directly as a binary response
        return StreamingResponse(stream_speech(), media_type="audio/mpeg", 
                                 headers={"Content-Disposition": "attachment; filename=audio.mp3"})

    except Exception as e:
        print(f"An error occurred: {e}")
        raise HTTPException(status_code=500, detail="Internal Server Error")

