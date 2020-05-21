@Service 
public class SecureStoreServiceImpl implements SecureStoreService {     
	private static final Logger LOGGER = LoggerFactory.getLogger(SecureStoreService.class);     
	
	@Autowired    
	@Qualifier("secureStoreDataSource")     
	private DataSource dataSource;     
	@Override     
	public void getValue(String storeName, String key) throws SQLException { 
    	LOGGER.info("secure store read value: " + storeName + " / " + key);        
		
		try (Connection connection = dataSource.getConnection()) {   
	        CallableStatement callableStatement = connection.prepareCall("{call SYS.USER_SECURESTORE_RETRIEVE(?,?,?,?)}");       
			callableStatement.setString(1, storeName);        
			callableStatement.setBoolean(2, false); 
	        callableStatement.setString(3, key);       
			callableStatement.registerOutParameter(4, Types.VARBINARY);              
			
			callableStatement.executeUpdate();       
			byte[] bytes = callableStatement.getBytes(4);
        	if (null == bytes) { 
            	LOGGER.info("value is null");              
				return null;        
			} 
			else {               
				String returnValue = new String(bytes);               
				LOGGER.info("return value is :" + returnValue);               
				return returnValue; 
	        }    
		}
 	} 
}
