import os
import io
import logging
from fastapi import FastAPI, HTTPException
from fastapi.middleware.cors import CORSMiddleware
from pydantic import BaseModel
from fastapi.responses import StreamingResponse
from gtts import gTTS
from openai import OpenAI
import time
from fastapi.testclient import TestClient  # Import TestClient

# Initialize the OpenAI client
client = OpenAI()
#OpenAI.api_key = os.getenv('OPENAI_API_KEY') # Replace with your OpenAI API key
OpenAI.api_key = os.getenv('OPENAI_API_KEY')  # Replace with your key if needed

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

   # Initialize the thread
existing_assistant_id = "asst_KwbkEYapMSuJDNHO6qGtyazI"

# Step 1: Retrieve the Existing Assistant
existing_assistant = client.beta.assistants.retrieve(existing_assistant_id)
print(f"This is the existing assistant object: {existing_assistant} \n")


# Create a TestClient instance for sending requests to the FastAPI app
client_app = TestClient(app)

try :
    # Step 2: Create a Thread
    my_thread = client.beta.threads.create()
    #print(f"This is the thread object: {my_thread} \n")

    # Track the time of the last assistant message to handle multiple inputs correctly
    last_message_time = 0
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

def run_assistant_on_thread(thread_id, assistant_id, instructions):
    """Runs the assistant on the specified thread and returns the run object."""
    run = client.beta.threads.runs.create(
        thread_id=thread_id,
        assistant_id=assistant_id,
        instructions=instructions
    )
    return run

def wait_for_run_completion(thread_id, run_id):
    """Waits for the assistant run to complete and returns the final status."""
    while True:
        keep_retrieving_run = client.beta.threads.runs.retrieve(
            thread_id=thread_id,
            run_id=run_id
        )
        if keep_retrieving_run.status == "completed":
            return keep_retrieving_run.status
        elif keep_retrieving_run.status in ["queued", "in_progress"]:
            time.sleep(2)  # Avoid hitting the API too frequently
        else:
            return keep_retrieving_run.status

def retrieve_latest_assistant_message(thread_id, last_message_time):
    """Retrieves the latest message from the assistant in the thread after the last known message time."""
    all_messages = client.beta.threads.messages.list(thread_id=thread_id)

    # Debugging: Print all messages to ensure we're seeing the complete list
    print(f"Total messages retrieved: {len(all_messages.data)}")
    for msg in all_messages.data:
        print(f"Message ID: {msg.id}, Role: {msg.role}, Content: {msg.content[0].text.value}")
    #ENDFOR

    # Find new assistant messages that are newer than the last known message time
    new_assistant_messages = [msg for msg in all_messages.data if msg.role == "assistant" and msg.created_at > last_message_time]

    # If there are new assistant messages, return the latest one
    if new_assistant_messages:
        latest_message = new_assistant_messages[-1]
        return latest_message.content[0].text.value, latest_message.created_at
    #ENDIF
    return None, last_message_time


@app.post("/chatinput")
async def stream_audio(request: TextRequest):

    # Add message
    add_message_to_thread(my_thread.id, "user", request.text)

    # Run the assistant
    my_run = run_assistant_on_thread(my_thread.id, existing_assistant_id, existing_assistant.instructions)

    # Wait for run to complete
    run_status = wait_for_run_completion(my_thread.id, my_run.id)


    if run_status == "completed":
        try:
            # Attempt to retrieve the latest assistant message
            assistant_message, last_message_time = retrieve_latest_assistant_message(my_thread.id, last_message_time)
        
            # Check if the assistant message was successfully retrieved
            if assistant_message is None:
                raise ValueError("Failed to retrieve assistant message: None returned")


        except ValueError as ve:
            print(f"ValueError occurred: {ve}")
    
        except RuntimeError as re:
            print(f"RuntimeError occurred: {re}")

        except Exception as e:
            # General exception catch for any unexpected errors
            print(f"An unexpected error occurred: {e}")
    #END IF

    try:
        user_text = assistant_message

        # Generate speech using gTTS
        tts = gTTS(text=user_text, lang='en')

        # Create a BytesIO stream to hold the MP3 data
        mp3_fp = io.BytesIO()
        tts.write_to_fp(mp3_fp)
        mp3_fp.seek(0)

        # Stream the MP3 file directly as a binary response
        return StreamingResponse(mp3_fp, media_type="audio/mpeg", 
                                 headers={"Content-Disposition": "attachment; filename=audio.mp3"})

    except Exception as e:
        print(f"An error occurred: {e}")
        raise HTTPException(status_code=500, detail="Internal Server Error")

