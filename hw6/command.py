import datetime
import re


IAMAT = "IAMAT"
WHATSAT = "WHATSAT"
FLOOD = "FLOOD"
UNKNOWN = "?"

ISO = r"[+-][1-9]\d*\.?\d*"
POSIX = re.compile("[1-9]\d*\.\d*")

SERVERS = [
	"Bernard", "Campbell", "Jaquez", "Juzang", "Riley"
]

#when creating a command object, it will have been trimmed already
class Command:
	def __init__(self, command):
		self.command = command
		self.tokens = command.split()
		self.type = UNKNOWN
	
	def get_tokens(self):
		return self.tokens
	
	def get_type(self):
		return self.type
	
	def get_command(self):
		return self.command

	def validate(self):
		print(self.tokens)
		if len(self.tokens) != 4 and len(self.tokens) != 6:
			return self.type
		#evaluate 1st token
		first_token = self.tokens[0]
		second_token = self.tokens[1]
		third_token = self.tokens[2]
		fourth_token = self.tokens[3]

		if first_token == IAMAT and len(self.tokens) == 4:
			if self.evaluate_iamat(third_token, fourth_token):
				return IAMAT
		elif first_token == WHATSAT and len(self.tokens) == 4:
			if self.evaluate_whatsat(third_token, fourth_token):
				return WHATSAT
		elif first_token == FLOOD and len(self.tokens) == 6:
			if self.evaluate_flood():
				return FLOOD	
		else: return UNKNOWN
	
	def evaluate_iamat(self, latlon, posix):
		return self.is_iso6709(latlon) != None and self.is_posix(posix)

	def evaluate_whatsat(self, radius, item_cap):
		try:
			float_rad = float(radius)
			float_cap = float(item_cap)
			if float_cap % int(float_cap) != 0:
				return False
			return float_rad >= 0 and float_rad <= 50 and float_cap >= 0 and float_cap <= 20
		except Exception as e:
			return False

	def evaluate_flood(self):
		return (self.is_iso6709(self.tokens[2]) != None) and self.is_posix(self.tokens[3]) and self.is_posix(self.tokens[4]) and (self.tokens[5] in SERVERS)

	def is_iso6709(self, latlon):
		coords = re.findall(ISO, latlon)
		print(coords)
		if not len(coords) == 2:
			return None
		try:
			lat = float(coords[0])
			lon = float(coords[1])
			print(lat, lon)
			if lat > 90 or lon > 180:
				return None
			return [lat,lon]
		except:
			return None
	
	def is_posix(self, posix):
		try:
			time = float(posix)
			datetime.datetime.utcfromtimestamp(time)
		except Exception as e:
			return False
		return POSIX.fullmatch(posix) != None
	