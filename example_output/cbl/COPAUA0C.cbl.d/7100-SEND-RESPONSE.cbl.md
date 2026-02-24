```cobol
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
      *----------------------------------------------------------------*        
                                                                                
      *- PENDING AUTHORIZATION SUMMARY SEGMENT - ROOT                           
       01 PENDING-AUTH-SUMMARY.                                                 
       COPY CIPAUSMY.                                                           
                                                                                
      *- PENDING AUTHORIZATION DETAILS SEGMENT - CHILD                          
       01 PENDING-AUTH-DETAILS.                                                 
       COPY CIPAUDTY.                                                           
                                                                                
      *----------------------------------------------------------------*        
      *DATASET LAYOUTS                                                          
      *----------------------------------------------------------------*        
      *- CARD XREF LAYOUT                                                       
       COPY CVACT03Y.                                                           
                                                                                
      *- ACCT RECORD LAYOUT                                                     
       COPY CVACT01Y.                                                           
                                                                                
      *- CUSTOMER LAYOUT                                                        
       COPY CVCUS01Y.                                                           
```
