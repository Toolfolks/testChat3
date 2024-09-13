# Install the required packages before running the code:
# pip install fastapi uvicorn openai gtts aiofiles

from fastapi import FastAPI, Request
from fastapi.responses import StreamingResponse
from openai import OpenAI
from gtts import gTTS
from openai import OpenAI
import asyncio
import aiofiles
import uuid
import os
from pydantic import BaseModel

from fastapi.testclient import TestClient  # Import TestClient
from fastapi.middleware.cors import CORSMiddleware


from openai import AssistantEventHandler
import threading
import queue

app = FastAPI()
    # Configure CORS to allow all origins (for development/testing purposes)

app.add_middleware(
    CORSMiddleware,
    allow_origins=["*"],  # Allow all origins
    allow_credentials=True,
    allow_methods=["*"],  # Allow all HTTP methods including OPTIONS
    allow_headers=["*"],  # Allow all headers
)


# Initialize the OpenAI client

client = OpenAI()
OpenAI.api_key = os.getenv('OPENAI_API_KEY')  

# Define the input model
class TextRequest(BaseModel):
   text: str

# Replace 'existing_assistant_id' with your actual assistant ID
existing_assistant_id = "asst_KwbkEYapMSuJDNHO6qGtyazI"

# Create a TestClient instance for sending requests to the FastAPI app
client_app = TestClient(app)

try :
    # Step 2: Create a Thread
    my_thread = client.beta.threads.create()
    #print(f"This is the thread object: {my_thread} \n")
except Exception as e:
    print(f"An thread error occurred: {e}")



def add_message_to_thread(thread_id, role, content):
    """Adds a message to the thread and returns the message object."""
    message = client.beta.threads.messages.create(
        thread_id=thread_id,
        role=role,
        content=content,
    )
    return message

def run_assistant(thread_id, assistant_id, instructions, eventHandler):    
    stream = client.beta.threads.runs.create(
    thread_id= thread_id,
    assistant_id=assistant_id,
    instructions=instructions,
    event_handler=eventHandler)

    return stream;



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

app = FastAPI()

@app.post("/chat")
async def chat(request: TextRequest):

    q = queue.Queue()
    handler = EventHandler(q)
   # Add message
    add_message_to_thread(my_thread.id, "user", request.text)

    my_assistant = client.beta.assistants.retrieve(existing_assistant_id)


    stream_q = run_assistant(my_thread.id, existing_assistant_id,my_assistant.instructions, handler)


    t = threading.Thread(target=run_stream, args=(stream_q, q))
    t.start()

    response = ""
    while True:
        item = q.get()
        if item is None:
            break
        response += item

    return {"response": response}