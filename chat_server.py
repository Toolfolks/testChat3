# Install the required packages before running the code:
# pip install fastapi uvicorn openai gtts aiofiles

from fastapi import FastAPI, Request
from fastapi.responses import StreamingResponse
from openai import OpenAI
from gtts import gTTS
import openai
import asyncio
import aiofiles
import uuid
import os
from pydantic import BaseModel

from fastapi.testclient import TestClient  # Import TestClient
from fastapi.middleware.cors import CORSMiddleware

app = FastAPI()

# Replace 'YOUR_OPENAI_API_KEY' with your actual OpenAI API key
openai.api_key = os.getenv('OPENAI_API_KEY') 

# Initialize the OpenAI client
client = OpenAI(api_key=openai.api_key)

# Replace 'existing_assistant_id' with your actual assistant ID
existing_assistant_id = "asst_KwbkEYapMSuJDNHO6qGtyazI"

# Define the input model
class TextRequest(BaseModel):
   text: str


@app.post("/chat")
async def chat(request: Request):
    data = await request.json()
    user_input = data.get('text', '')

    
    # Configure CORS to allow all origins (for development/testing purposes)
    app.add_middleware(
        CORSMiddleware,
        allow_origins=["*"],  # Allow all origins
        allow_credentials=True,
        allow_methods=["*"],  # Allow all HTTP methods including OPTIONS
        allow_headers=["*"],  # Allow all headers
    )

    # Create a TestClient instance for sending requests to the FastAPI app
    client_app = TestClient(app)
    
    # Step 1: Retrieve the Existing Assistant
    existing_assistant = client.beta.assistants.retrieve(existing_assistant_id)

    async def stream_chat():


        # Start a chat session with the assistant
        response = await existing_assistant.chat_create(
            messages=[{"role": "user", "content": user_input}],
            stream=True
        )

        # Initialize text content accumulator
        text_content = ''

        # Stream the assistant's response back to the client
        async for chunk in response:
            if 'choices' in chunk:
                choice = chunk['choices'][0]
                if 'delta' in choice:
                    delta = choice['delta']
                    if 'content' in delta:
                        content = delta['content']
                        text_content += content
                        yield content

        # After streaming text, convert accumulated text to speech
        tts = gTTS(text=text_content, lang='en')
        audio_file = f"{uuid.uuid4()}.mp3"
        tts.save(audio_file)

        # Stream the audio file back to the client
        async with aiofiles.open(audio_file, mode='rb') as f:
            audio_data = await f.read()
            yield audio_data

        # Clean up the audio file
        os.remove(audio_file)

    return StreamingResponse(stream_chat(), media_type="text/plain")
