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
    # Configure CORS to allow all origins (for development/testing purposes)

app.add_middleware(
    CORSMiddleware,
    allow_origins=["*"],  # Allow all origins
    allow_credentials=True,
    allow_methods=["*"],  # Allow all HTTP methods including OPTIONS
    allow_headers=["*"],  # Allow all headers
)

# Replace 'YOUR_OPENAI_API_KEY' with your actual OpenAI API key
openai.api_key = os.getenv('OPENAI_API_KEY') 

# Initialize the OpenAI client
client = OpenAI(api_key=openai.api_key)

# Replace 'existing_assistant_id' with your actual assistant ID
existing_assistant_id = "asst_KwbkEYapMSuJDNHO6qGtyazI"

# Define the input model
class TextRequest(BaseModel):
   text: str

from flask import Flask, request, Response
import openai
from openai import AssistantEventHandler
import threading
import queue

app = Flask(__name__)

# Replace 'YOUR_API_KEY' with your actual OpenAI API key
openai.api_key = 'YOUR_API_KEY'

class EventHandler(AssistantEventHandler):
    def __init__(self, q):
        self.q = q

    def on_text_created(self, text) -> None:
        self.q.put("\nassistant > ")

    def on_text_delta(self, delta, snapshot):
        self.q.put(delta.value)

    def on_tool_call_created(self, tool_call):
        self.q.put(f"\nassistant > {tool_call.type}\n")
      
    def on_tool_call_delta(self, delta, snapshot):
        if delta.type == 'code_interpreter':
            if delta.code_interpreter.input:
                self.q.put(delta.code_interpreter.input)
            if delta.code_interpreter.outputs:
                self.q.put(f"\n\noutput >")
                for output in delta.code_interpreter.outputs:
                    if output.type == "logs":
                        self.q.put(f"\n{output.logs}")

def run_stream(stream, q):
    stream.until_done()
    q.put(None)  # Signal the end of the stream

@app.route('/chat', methods=['POST'])
def chat():
    data = request.json
    user_input = data.get("input", "")

    # Replace 'YOUR_ASSISTANT_ID' with your actual assistant ID
    assistant_id = 'YOUR_ASSISTANT_ID'

    # Create a new thread for the conversation
    thread = openai.beta.threads.create(assistant_id=assistant_id)
    thread_id = thread.id

    q = queue.Queue()
    handler = EventHandler(q)

    # Start the streaming run
    stream = openai.beta.threads.runs.stream(
        thread_id=thread_id,
        assistant_id=assistant_id,
        messages=[{"role": "user", "content": user_input}],
        event_handler=handler,
    )

    # Run the stream in a separate thread
    t = threading.Thread(target=run_stream, args=(stream, q))
    t.start()

    def generate():
        while True:
            item = q.get()
            if item is None:
                break
            yield item

    return Response(generate(), mimetype='text/plain')

if __name__ == '__main__':
    app.run(debug=True)
