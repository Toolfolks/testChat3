import os

def check_ffmpeg_path(path):
  """Checks if the specified FFmpeg path exists and is executable.

  Args:
    path: The path to the FFmpeg executable.

  Returns:
    True if the path is valid, False otherwise.
  """

  if os.path.exists(path) and os.access(path, os.X_OK):
    return True
  else:
    return False

# Example usage:
ffmpeg_path = "/home/wilseayd/dementiaChat/ffmpeg/ffmpeg"
if check_ffmpeg_path(ffmpeg_path):
  print("FFmpeg found at:", ffmpeg_path)
else:
  print("FFmpeg not found or not executable at:", ffmpeg_path)