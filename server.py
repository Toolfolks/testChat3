import os
import io
import logging
import base64
from fastapi import FastAPI, HTTPException
from fastapi.middleware.cors import CORSMiddleware
from pydantic import BaseModel
from gtts import gTTS

# Set up FastAPI
app = FastAPI()

# Configure CORS to allow requests from specific origins
app.add_middleware(
    CORSMiddleware,
    allow_origins=["https://wilsea.com"],  # Allow only specific origin
    allow_credentials=True,
    allow_methods=["*"],  # Allow all HTTP methods including OPTIONS
    allow_headers=["*"],  # Allow all headers
)

# Set up logging
logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)

# Define the input model
class TextRequest(BaseModel):
    text: str

@app.post("/stream")
async def stream_audio(request: TextRequest):
    try:
        user_text = request.text

        # Generate speech using gTTS
        tts = gTTS(text=user_text, lang='en')

        # Create a BytesIO stream to hold the MP3 data
        mp3_fp = io.BytesIO()
        tts.write_to_fp(mp3_fp)
        mp3_fp.seek(0)

        # Encode the MP3 data as base64
        base64_mp3 = base64.b64encode(mp3_fp.getvalue()).decode('utf-8')

        return base64_mp3  # Return the base64-encoded string

    except Exception as e:
        logger.error(f"An error occurred: {e}")
        raise HTTPException(status_code=500, detail="Internal Server Error")

# Example of running the server: uvicorn server:app --host 0.0.0.0 --port 8000
