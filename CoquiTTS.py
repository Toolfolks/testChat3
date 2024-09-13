import os
import io
import logging
from fastapi import FastAPI, HTTPException
from fastapi.middleware.cors import CORSMiddleware
from pydantic import BaseModel
from fastapi.responses import StreamingResponse
from gtts import gTTS
import openai
import asyncio

# Initialize logging
logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)

# Initialize OpenAI API key
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

@app.post("/chatinput")
async def stream_audio(request: TextRequest):
    try:
        # Send user message to OpenAI
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

    async def generate_audio_stream():
        buffer = ""
        try:
            async for chunk in response:
                if 'choices' in chunk:
                    delta = chunk['choices'][0]['delta']
                    if 'content' in delta:
                        buffer += delta['content']
                        # Yield the text chunks as they come
                        logger.debug(f"Received chunk: {delta['content']}")
                        yield delta['content'].encode('utf-8')  # Yield bytes
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
