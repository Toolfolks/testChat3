import os
from openai import OpenAI
import time

# Initialize the OpenAI client
client = OpenAI()
OpenAI.api_key = os.getenv('OPENAI_API_KEY')  # Replace with your key if needed

# Specify the ID of the existing assistant
existing_assistant_id = "asst_KwbkEYapMSuJDNHO6qGtyazI"

# Step 1: Retrieve the Existing Assistant
existing_assistant = client.beta.assistants.retrieve(existing_assistant_id)
print(f"This is the existing assistant object: {existing_assistant} \n")

# Step 2: Create a Thread
my_thread = client.beta.threads.create()
print(f"This is the thread object: {my_thread} \n")

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

def retrieve_latest_assistant_message(thread_id, last_message_time):
    """Retrieves the latest message from the assistant in the thread after the last known message time."""
    all_messages = client.beta.threads.messages.list(thread_id=thread_id)

    # Debugging: Print all messages to ensure we're seeing the complete list
    print(f"Total messages retrieved: {len(all_messages.data)}")
    for msg in all_messages.data:
        print(f"Message ID: {msg.id}, Role: {msg.role}, Content: {msg.content[0].text.value}")

    # Find new assistant messages that are newer than the last known message time
    new_assistant_messages = [msg for msg in all_messages.data if msg.role == "assistant" and msg.created_at > last_message_time]

    # If there are new assistant messages, return the latest one
    if new_assistant_messages:
        latest_message = new_assistant_messages[-1]
        return latest_message.content[0].text.value, latest_message.created_at
    return None, last_message_time

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

# Track the time of the last assistant message to handle multiple inputs correctly
last_message_time = 0

# Initial user input and assistant run
user_content = input("Enter your message to the assistant: ")
add_message_to_thread(my_thread.id, "user", user_content)

# Run the assistant
my_run = run_assistant_on_thread(my_thread.id, existing_assistant_id, existing_assistant.instructions)

# Wait for run to complete
run_status = wait_for_run_completion(my_thread.id, my_run.id)

if run_status == "completed":
    assistant_message, last_message_time = retrieve_latest_assistant_message(my_thread.id, last_message_time)
    print(f"Assistant: {assistant_message}")

# Continue conversation
while True:
    user_content = input("Enter your next message to the assistant (or type 'exit' to quit): ")
    if user_content.lower() == 'exit':
        break

    # Add user message to the thread
    add_message_to_thread(my_thread.id, "user", user_content)

    # Run the assistant
    my_run = run_assistant_on_thread(my_thread.id, existing_assistant_id, existing_assistant.instructions)

    # Wait for the assistant to process the message
    run_status = wait_for_run_completion(my_thread.id, my_run.id)

    if run_status == "completed":
        assistant_message, last_message_time = retrieve_latest_assistant_message(my_thread.id, last_message_time)
        print(f"Assistant: {assistant_message}")
