## Purpose: wrapper to pyhdb to simplify the query call
## Date   : Nov 11, 2019
## Author : Simon Li
'''
Usage  : 
from MyHdb import MyHdb 
hdb = MyHdb() 
'''

#https://github.com/SAP/PyHDB
import pyhdb

class MyHdb:
   #constructor
   def __init__(self, sys = 'SB3'):
      self.sys = sys.upper()
   
      self.__login = {}
      self.__setConnection(sys)

   #destructor
   def __del__(self):
        self.__connection.close()

   # set the connection accordingly
   def __setConnection(self, sys):
      self.__login['user'] = "shaw1236"
      self.__login['password'] = "testCQ3" # Will drop it from login
 	
      if self.sys == 'SB3':
         self.__login['host'] = r"10.230.82.120"  #sapr3sb3
         self.__login['instanceNo'] = "01"
      elif self.sys == "CD1":
         pass
      elif self.sys == "CQ1":
         pass
      elif self.sys == "CD2":
         pass
      elif self.sys == "CQ2":
         pass
      elif self.sys == "SB1":
         pass
      elif self.sys == "SB2":
         pass
      else:
         raise ValueError("System id {0} is not supported!".format(sys))
     
      self.__login['port'] = 30015 + 100 * int(self.__login['instanceNo'])
      #print(self.__login)

      self.__schema = "SAPSB3"
         
      self.__connection = pyhdb.connect(
         host = self.__login['host'],
         port = self.__login['port'],
         user = self.__login['user'],
         password = self.__login['password']
      )
      self.__cursor = self.__connection.cursor()

   @property
   def schema(self):
      return self.__schema 
   
   @schema.setter
   def schema(self, schema):
      self.__schema = schema
  
   @property
   def connection(self):
      return self.__connection 
   
   @connection.setter
   def connection(self, conn):
      pass

   @property
   def cursor(self):
      return self.__cursor

   def execute(self, isql):
      sql =  isql.replace('$.', self.__schema)
      sql =  sql.replace('$', self.__schema)
      self.__cursor.execute(sql) 

   def fetchmany(self, num):
      return self.__cursor.fetchmany(num) 

   def fetchone(self):
      return self.__cursor.fetchone() 
      
   def fetchall(self):
      return self.__cursor.fetchall() 

if __name__ == '__main__':
   #from MyHdb import MyHdb 
   
   try:     
      hdb = MyHdb()
      hdb.execute("SELECT 'Hello Python World' FROM DUMMY")
      print(hdb.fetchone())

      hdb.execute("SELECT pstyv, kurztext from $TJHAPT")
      records = hdb.fetchmany(3)   #fetchone(), fetchall()
      print(records)

   except ValueError as e:
      print(str(e))   