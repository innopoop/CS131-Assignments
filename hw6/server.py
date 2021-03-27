import command as cmd
import asyncio
import aiohttp
import logging
import time
import json

#Port assignments: 15875--15879

GOOGLE_API_KEY = "OMITTED"
REQUEST_URL = "https://maps.googleapis.com/maps/api/place/nearbysearch/json?"
HOST = "127.0.0.1"


SERVER_COMMUNICATION = {
	"Bernard": ["Jaquez", "Juzang", "Campbell"],
	"Campbell": ["Bernard", "Juzang"],
	"Jaquez": ["Riley", "Bernard"],
	"Juzang": ["Campbell", "Bernard", "Riley"],
	"Riley": ["Jaquez", "Juzang"],
}

SERVER_PORTS = {
	"Bernard": 15875,
	"Campbell": 15876,
	"Jaquez": 15877,
	"Juzang": 15878,
	"Riley": 15879,
}

def time_difference(sent, received):
	time_diff = float(received) - float(sent)
	if time_diff < 0:
		return str(time_diff)
	else:
		return '+' + str(time_diff)

class Server:
	def __init__(self, server_name):
		self.name = server_name
		self.port = SERVER_PORTS[server_name]
		self.neighbors = SERVER_COMMUNICATION[server_name]
		self.clients = {}

		#for logging
		logging.basicConfig(level=logging.DEBUG, filename=f"{server_name}.log", format="%(asctime)s %(message)s", filemode="w")
		self.logger = logging.StreamHandler()
		self.logger.setLevel(logging.INFO)
		logging.getLogger('').addHandler(self.logger)

	async def respondIAMAT(self, command, callback_time, writer):
		tokens = command.get_tokens()
		client = tokens[1]
		coords = tokens[2]
		sent_time = tokens[3]
		time_diff = time_difference(sent_time, callback_time)

		command_details = " ".join(tokens[1:])

		self.clients[client] = [coords, command_details, sent_time, callback_time, self.name ]

		response = f"AT {self.name} {time_diff} {command_details}"
		#FORMAT: FLOOD kiwi.cs.ucla.edu +34.068930-118.445127 1614209128.918963997 [server_received] Riley
		update = f"FLOOD {command_details} {callback_time} {self.name}\n"
		asyncio.ensure_future(self.flood(update))
		await self.send_response(response, writer)

	async def respondWHATSAT(self, command, writer):
		tokens = command.get_tokens()
		client = tokens[1]
		radius = float(tokens[2])
		limit = int(tokens[3])

		coordinates, command_details, sent_time, callback_time, server = self.clients[client]
		time_diff = time_difference(sent_time, callback_time)
		lat, lon = command.is_iso6709(coordinates)
		location = str(lat) + "," + str(lon) 
		radius_km = radius * 1000

		#FORMAT: https://maps.googleapis.com/maps/api/place/nearbysearch/json?location=34.068930,-118.445127&radius=10000&key=AIzaSyAcFXjOaI1w1MDNBpLAvakTlI45rW9bR_U
		request = REQUEST_URL + f"location={location}&radius={radius_km}&key={GOOGLE_API_KEY}"

		at_response = f"AT {server} {time_diff} {command_details}"
		places_json = {}
		async with aiohttp.ClientSession() as session:
			response = await session.request(method="GET", url=request)
			places_json = await response.json()
		
		places_json['results'] = places_json['results'][:limit]
		str_json = json.dumps(places_json, indent=3)
		server_response = at_response + "\n" + str_json
		await self.send_response(server_response, writer)

	async def handleFLOOD(self, command):
		#FORMAT: FLOOD kiwi.cs.ucla.edu +34.068930-118.445127 1614209128.918963997 [server_accept_time] Riley

		tokens = command.get_tokens()
		client = tokens[1]
		coords = tokens[2]
		client_sent = tokens[3]
		callback_time = float(tokens[4])
		server = tokens[5]

		if client in self.clients:
			last_received = float(self.clients[client][3])
			if callback_time > last_received:
				self.clients[client] = [coords, "".join(tokens[2:4]), client_sent, callback_time, server]
				# asyncio.ensure_future(self.flood(command.get_command()))
		else:
			self.clients[client] = [coords, "".join(tokens[2:4]), client_sent, callback_time, server]
			asyncio.ensure_future(self.flood(command.get_command()))

	async def flood(self, command):
		for server in self.neighbors:
			self.log(f"Connecting to {server} through port {self.port}.\n")
			try:
				reader, writer = await asyncio.open_connection(host=HOST, port=SERVER_PORTS[server])
				self.log("Connection established.\n")
				await self.send_response(command, writer)
			except:
				self.log("Failed to make connection.\n")
				pass

	async def client_connected_cb(self, reader, writer):
		callback_time = time.time()
		data = await reader.readline()
		command = data.decode()

		self.log(f"Received: {command}")

		clean_command = cmd.Command(command.strip())
		command_type = clean_command.validate()

		if command_type == cmd.IAMAT:
			await self.respondIAMAT(clean_command, callback_time, writer)
		elif command_type == cmd.WHATSAT and clean_command.get_tokens()[1] in self.clients.keys():
			await self.respondWHATSAT(clean_command, writer)
		elif command_type == cmd.FLOOD:
			await self.handleFLOOD(clean_command)
		else:
			await self.send_response(f"? {clean_command.get_command()}", writer)
	
	async def send_response(self, command, writer):
		self.log(f"Sending: {command}\n")
		writer.write(command.encode())
		await writer.drain()
		writer.write_eof()
		writer.close()

	def log(self, message):
		logging.info(message)

	def get_port(self):
		return self.port

async def main():
	import sys

	#check for correct arguments
	no_args = len(sys.argv)
	if not no_args == 2:
		print("Invalid number of arguments. Proper Usage: python3 server.py server_name.\n")
		exit(1)
	
	server_name = sys.argv[1]
	if not server_name in SERVER_PORTS.keys():
		print(f"Server name {server_name} does not match familiar names Bernard, Campbell, Jaquez, Juzang, or Riley.\n")
		exit(1)

	#create Server
	server_details = Server(server_name)

	#start the server
	server = await asyncio.start_server(server_details.client_connected_cb, HOST, server_details.get_port())
	server_details.log(f"Starting server {server_name} on {server_details.get_port()}\n")
	try:
		await server.serve_forever()
	except KeyboardInterrupt as e:
		pass
	server_details.log(f"Closing server {server_name} on {server_details.get_port()}\n")
	server.close()
	await server.wait_closed()

if __name__ == "__main__":
	asyncio.run(main())