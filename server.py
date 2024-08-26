import os
import io
import logging
from fastapi import FastAPI, HTTPException
from pydantic import BaseModel
from fastapi.responses import StreamingResponse
from gtts import gTTS
from pydub import AudioSegment

# Set environment variables for ffmpeg and ffprobe
os.environ['FFMPEG_BINARY'] = 'ffmpeg'  # Use ffmpeg directly from the installed location
os.environ['FFPROBE_BINARY'] = 'ffprobe'

app = FastAPI()
AudioSegment.converter = os.environ['FFMPEG_BINARY']
AudioSegment.ffprobe = os.environ['FFPROBE_BINARY']

# Set up logging
logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)

class TextRequest(BaseModel):
    text: str

@app.post("/stream")
async def stream_audio(request: TextRequest):
    try:
        user_text = request.text

        # Generate speech using gTTS
        tts = gTTS(text=user_text, lang='en')
        mp3_fp = io.BytesIO()
        tts.write_to_fp(mp3_fp)
        mp3_fp.seek(0)

        # Convert MP3 to WAV using pydub
        audio = AudioSegment.from_file(mp3_fp, format="mp3")
        wav_fp = io.BytesIO()
        audio.export(wav_fp, format="wav")
        wav_fp.seek(0)

        def iterfile():
            yield from wav_fp

        return StreamingResponse(iterfile(), media_type="audio/wav")
    except Exception as e:
        logger.error(f"An error occurred: {e}")
        raise HTTPException(status_code=500, detail="Internal Server Error")
