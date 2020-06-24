package test.securestore.controller;

import java.sql.SQLException; 
import org.springframework.beans.factory.annotation.Autowired; 
import org.springframework.web.bind.annotation.DeleteMapping; 
import org.springframework.web.bind.annotation.GetMapping; 
import org.springframework.web.bind.annotation.PostMapping; 
import org.springframework.web.bind.annotation.RequestMapping; 
import org.springframework.web.bind.annotation.RestController; 

import test.securestore.service.SecureStoreServiceImpl; 

@RestController 
@RequestMapping("/securestore") 
public class SecureStoreController {    
	@Autowired    
	private SecureStoreServiceImpl secureStoreService;    

	@PostMapping    
	public void setValue() throws SQLException {        
		secureStoreService.writeValue("store", "key", "value");
    }    

	@GetMapping    
	public String get() throws SQLException {  
	   return secureStoreService.getValue("store", "key");    
	}    

	@DeleteMapping    
	public void delete() throws SQLException {        
		secureStoreService.deleteValue("store", "key");    
	}
}

