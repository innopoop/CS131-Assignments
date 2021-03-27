import asyncio
import time
import sys

SERVER_PORTS = {
	"Bernard": 15875,
	"Campbell": 15876,
	"Jaquez": 15877,
	"Juzang": 15878,
	"Riley": 15879,
}

async def send_command(command, writer):
	print(f'Send: {command!r}')
	writer.write(command.encode())
	await writer.drain()

async def receive_command(reader):
	data = await reader.read(100000000)
	print(f'Received: {data.decode()!r}')
	
async def tcp_client_query(command, port, loop):
	try:
		reader, writer = await asyncio.open_connection('127.0.0.1', port, loop=loop)
	except ConnectionRefusedError:
		return
	await send_command(command, writer)
	await receive_command(reader)

if __name__ == "__main__":
	if(len(sys.argv) != 2):
		print("Invalid number of arguments. Proper usage example python3 client.py client_name.")
		exit(1)
	
	client_name = sys.argv[1]
	command_iamat = f"     IAMAT {client_name} +34.065445-118.444732 {time.time()}\t\n\nhello\n\t"
	command_whatsat = f" WHATSAT {client_name} 10 5\n"
	command_error = "IAMAT badlyformatted command\n"
	event_loop = asyncio.get_event_loop()
	event_loop.run_until_complete(tcp_client_query(command_iamat, SERVER_PORTS["Riley"], event_loop))
	# event_loop.run_until_complete(tcp_client_query(command_whatsat, SERVER_PORTS["Riley"], event_loop))
	event_loop.run_until_complete(tcp_client_query(command_error, SERVER_PORTS["Riley"], event_loop))
	event_loop.run_until_complete(tcp_client_query(command_whatsat, SERVER_PORTS["Jaquez"], event_loop))
	event_loop.close()
