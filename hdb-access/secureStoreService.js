/*eslint no-console: 0, no-unused-vars: 0, no-shadow: 0, new-cap: 0*/

"use strict"; 

const Promise = require('bluebird');
const hdbext = Promise.promisifyAll(require("@sap/hdbext"));          

class SecureStoreService {
    static getSecureStore() {    
	    return new Promise((resolve, reject) => {       
		   const xsenv = require("@sap/xsenv");            
		   let hanaOptions = xsenv.getServices({       
	    	    secureStore: {plan: "securestore"}            
		   });            
		   const hdbext = require("@sap/hdbext");            
		   hdbext.createConnection(hanaOptions.secureStore, (error, client) => {    
	          if (error) {        
		          reject(error); 
              } else {      
	              resolve(client);                
	          }            
		   });       
       });    
    }
	
	static validateKey(key, nThrow = true) {
		if (typeof key === "undefined" || key === null)) {
            if (nThrow)           
	       		throw new Error("ERROR: No Secure Store Key Input Parameter Specified");  
           	return false;          
		}
		return true; 
 	}

    constructor(storeName = 'MySecureStore') {
        this.store = storeName;

		// Secure Store Insert
    	this.add = async (key, value) => {
	    	SecureStoreService.validateKey(key);

			const inputParams = {
                 KEY: key,          
        		 STORE_NAME: this.store,         
        	     FOR_XS_APPLICATIONUSER:true,         
        		 VALUE: value
			}          
        	const client = await SecureStoreService.getSecureStore();    

        	//(client, Schema, Procedure, callback)
        	const sp = await hdbext.loadProcedure(client, "SYS", "USER_SECURESTORE_INSERT");

            //(Input Parameters, callback(errors, Output Scalar Parameters, [Output Table Parameters])       
            const ret = await sp(inputParams);
            return ret;
		}); 

    	// Secure Store Retrieve     
		this.get = async key => {     
            SecureStoreService.validateKey(key);
 
        	const client = await SecureStoreService.getSecureStore();    
            //(client, Schema, Procedure, callback)
		    const sp = await hdbext.loadProcedure(client, "SYS", "USER_SECURESTORE_RETRIEVE");

            let inputParams = {KEY: key};          
            inputParams.STORE_NAME = this.store;         
            inputParams.FOR_XS_APPLICATIONUSER = true;                 
   
			//(Input Parameters, callback(errors, Output Scalar Parameters, [Output Table Parameters])         
		    const ret = await sp(inputParams); 

            let value = null;
            if (ret && ret.results && ret.results.VALUE)
               value = ret.results.VALUE.toString("utf8);
            return value;
	    });     
	   

	    // Secure Store Delete     
	    this.delete = async key => {
            SecureStoreService.validateKey(key);

        	let inputParams = {KEY: key};          
			inputParams.STORE_NAME = this.store;      
			inputParams.FOR_XS_APPLICATIONUSER = true;      
			const client = await SecureStoreService.getSecureStore();     

        	//(client, Schema, Procedure, callback)     
			const sp = await hdbext.loadProcedure(client, "SYS", "USER_SECURESTORE_DELETE");
            const ret = await sp(inputParams);
            return ret;
        });       
	}
}     

module.exports = SecureStoreService;
