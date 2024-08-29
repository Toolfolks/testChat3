import os
import io
import logging
from fastapi import FastAPI, HTTPException
from fastapi.middleware.cors import CORSMiddleware
from pydantic import BaseModel
from fastapi.responses import StreamingResponse
from gtts import gTTS

# Set up FastAPI
app = FastAPI()

# Configure CORS to allow all origins (for development/testing purposes)
app.add_middleware(
    CORSMiddleware,
    allow_origins=["*"],  # Allow all origins
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

        # Stream the MP3 file directly as a binary response
        return StreamingResponse(mp3_fp, media_type="audio/mpeg", headers={
            "Content-Disposition": "attachment; filename=audio.mp3"
        })

    except Exception as e:
        logger.error(f"An error occurred: {e}")
        raise HTTPException(status_code=500, detail="Internal Server Error")

# Example of running the server: uvicorn server:app --host 0.0.0.0 --port 8000
