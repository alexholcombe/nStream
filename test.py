import numpy as np

n_text = 24
nStreams = 21

stream_positions = np.repeat([np.arange(n_text)],nStreams, axis = 0)

for stream in range(nStreams):
	new_stream = np.random.choice(stream_positions[stream], size = n_text, replace = False)
	matched = True
	if stream is not 0:
		while matched:
			for existing_stream in range(stream):
				if np.all(new_stream == stream_positions[existing_stream]):
					new_stream = np.random.choice(stream_positions[stream], size = n_text, replace = False)
				elif (existing_stream) == (stream - 1):
					matched = False
	stream_positions[stream,...] = new_stream


print(stream_positions)