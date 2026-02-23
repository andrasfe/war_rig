           COPY CMQTML.                                                         
                                                                                
       01  MQM-PUT-MESSAGE-OPTIONS.                                             
           COPY CMQPMOV.                                                        
                                                                                
       01  MQM-GET-MESSAGE-OPTIONS.                                             
           COPY CMQGMOV.                                                        
                                                                                
      *----------------------------------------------------------------*        
      *  STAGING COPYBOOKS                                                      
      *----------------------------------------------------------------*        
                                                                                
      *- PENDING AUTHORIZATION REQUEST LAYOUT                                   
       01 PENDING-AUTH-REQUEST.                                                 
       COPY CCPAURQY.                                                           
                                                                                
      *- PENDING AUTHORIZATION RESPONSE LAYOUT                                  
       01 PENDING-AUTH-RESPONSE.                                                
       COPY CCPAURLY.                                                           
                                                                                
      *- APPLICTION ERROR LOG LAYOUT                                            
       COPY CCPAUERY.                                                           
                                                                                
      *----------------------------------------------------------------*        
      *  IMS SEGMENT LAYOUT                                                     
