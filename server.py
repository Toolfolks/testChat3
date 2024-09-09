import os
from openai import OpenAI
import time

# Define the filename
filename = 'myFile.txt'

# Define the content to write
content = '1234'

# Open the file in write mode and write the content
with open(filename, 'w') as file:
    file.write(content)

print(f"{filename} has been created with the content: {content}")
