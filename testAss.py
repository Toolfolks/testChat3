import os
import time
from openai import OpenAI

# Set your OpenAI API key
OpenAI.api_key = os.getenv('OPENAI_API_KEY')

# Create an OpenAI client
client = OpenAI()

# Use the existing assistant ID
assistant_id = "asst_KwbkEYapMSuJDNHO6qGtyazI"

def send_to_assistant(text):
    """Sends text to an OpenAI assistant and returns the response."""

    try:
        # Create a Thread
        my_thread = client.beta.threads.create()
        print(f"This is the thread object: {my_thread} \n")

        # Add a Message to the Thread
        my_thread_message = client.beta.threads.messages.create(
            thread_id=my_thread.id,
            role="user",
            content=text
        )
        print(f"This is the message object: {my_thread_message} \n")

        # Run the Assistant
        my_run = client.beta.threads.runs.create(
            thread_id=my_thread.id,
            assistant_id=assistant_id
        )
        print(f"This is the run object: {my_run} \n")

        # Periodically retrieve the Run to check on its status to see if it has moved to completed
        while my_run.status in ["queued", "in_progress"]:
            keep_retrieving_run = client.beta.threads.runs.retrieve(
                thread_id=my_thread.id,
                run_id=my_run.id
            )
            print(f"Run status: {keep_retrieving_run.status}")

            if keep_retrieving_run.status == "completed":
                print("\n")

                # Retrieve the Messages added by the Assistant to the Thread
                all_messages = client.beta.threads.messages.list(
                    thread_id=my_thread.id
                )

                print("------------------------------------------------------------ \n")

                for message in all_messages.data:
                    if message.role == 'user':
                        print(f"User: {message.content[0].text.value}")
                    elif message.role == 'assistant':
                        print(f"Assistant: {message.content[0].text.value}")

                break
            elif keep_retrieving_run.status in ["queued", "in_progress"]:
                time.sleep(2)  # Wait for 2 seconds before checking again
            else:
                print(f"Run status: {keep_retrieving_run.status}")
                break

    except Exception as e:
        print(f"An error occurred: {e}")

# Example usage
text = "Who are you?"
send_to_assistant(text)
