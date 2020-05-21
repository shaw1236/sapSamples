import os 
import json 
from cfenv import AppEnv 
from hdbcli import dbapi 

class SecureStoreService:
	def __init__(self, storeName = 'TestStoreName'):

		env = AppEnv()
		port = int(os.getenv("PORT", 9099)) 
		hana = env.get_service(label='hana')
    
        self.__connection = None
        self.__store = storeName

		self.__hana['schema']   = hana.credentials['schema']    
		self.__hana['host']     = hana.credentials['host']    
		self.__hana['port']     = int(hana.credentials['port'])    
		self.__hana['user']     = hana.credentials['user']    
		self.__hana['password'] = hana.credentials['password']    
	   
		# Certificate for SAP HANA service instances that require an encrypted connection    
		if 'certificate' in hana.credentials:        
			self.__hana['haascert'] = hana.credentials['certificate']        
			self.__connection = dbapi.connect(
	 						        address=self.__hana['host'],
	         						port=self.__hana['port'],
	         						user=self.__hana['user'],
	         						password=self.__hana['password'],
							        currentSchema=self.__hana['schema'],
							        encrypt="true",
							        sslValidateCertificate="true",
						            sslCryptoProvider="openssl",
							        sslTrustStore=self.__hana['haascert']
	     	)   
		else: 
	    	self.__connection = dbapi.connect(         
									address=self.__hana['host'],
							        port=self.__hana['port'],
							        user=self.__hana['user'], 
							        password=self.__hana['password'],
							        currentSchema=self.__hana['schema']
       		)
    	assert(self__connection is not None)

    def __del__(self): # Destructor
        if self.__connection is not None:
			self.__connection.close
            self.__connection = None   

    @property
    def connection(self):
    	return self.__connection

    @property
    def cursor(self):
		return self.__connection.cursor()


    def add(self, key = "TestKey", value = "Whatever"):
		# Prepare a cursor for SQL execution
    	cursor = self.connection.cursor() 
		# Form an SQL statement to retrieve some data 
 		string2store = value   
		import codecs    
		hex2store = (codecs.encode(str.encode(string2store), "hex")).decode()
        result = False
    	try: 
       		cursor.callproc("SYS.USER_SECURESTORE_INSERT", (self.__store, False, key, hex2store))
            result = True
		except:        
			print('key TestKey likely already exists. Try deleting first.\n') 

		#self.connection.close()    
		# Return the results
    	return result
 
	def get(self, key = "TestKey"): 
	    # Prepare a cursor for SQL execution    
		cursor = self.connection.cursor() 
	    # Form an SQL statement to retrieve some data    
		hexvalue = cursor.callproc("SYS.USER_SECURESTORE_RETRIEVE", (self.__store, False, key, None)) 
    	# Close the DB connection    
		#self.connection.close()    

		import codecs    
        retrieved = None
		if hexvalue[3] is None:
			pass
    	else:
        	retrieved = codecs.decode(hexvalue[3].hex(), "hex").decode()        
		# Return the results
     	return retrieved

	def securestore_delete(self, key = "TestKey"): 
		# Prepare a cursor for SQL execution    
		cursor = self.connection.cursor() 
    	# Form an SQL statement
		result = False
		try:     
			cursor.callproc("SYS.USER_SECURESTORE_DELETE", (self.__store, False, key))
			result = True
		except:
			print("No deletion was done")

	    # Close the DB connection    
		#self.connection.close()        
		# Return the results
    	return result

if __name__ = '__main__':
	store = SecureStoreService("secure_store")
    store.add('name', 'test')
    print(store.get('name'))
    store.delete('name')
