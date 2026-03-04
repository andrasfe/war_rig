```cobol
       01  W01-HOBJ-REQUEST            PIC S9(9) BINARY.                        
       01  W01-BUFFLEN                 PIC S9(9) BINARY.                        
       01  W01-DATALEN                 PIC S9(9) BINARY.                        
       01  W01-GET-BUFFER              PIC X(500).                              
                                                                                
       01  W02-HCONN-REPLY             PIC S9(9) BINARY VALUE ZERO.             
       01  W02-BUFFLEN                 PIC S9(9) BINARY.                        
       01  W02-DATALEN                 PIC S9(9) BINARY.                        
       01  W02-PUT-BUFFER              PIC X(200).                              
                                                                                
       01  WS-SWITCHES.                                                         
           05 WS-AUTH-RESP-FLG         PIC X(01).                               
              88 AUTH-RESP-APPROVED    VALUE 'A'.                               
              88 AUTH-RESP-DECLINED    VALUE 'D'.                               
           05 WS-MSG-LOOP-FLG          PIC X(01) VALUE 'N'.                     
              88 WS-LOOP-END           VALUE 'E'.                               
           05 WS-MSG-AVAILABLE-FLG     PIC X(01) VALUE 'M'.                     
              88 NO-MORE-MSG-AVAILABLE VALUE 'N'.                               
              88 MORE-MSG-AVAILABLE    VALUE 'M'.                               
           05 WS-REQUEST-MQ-FLG        PIC X(01) VALUE 'C'.                     
              88 WS-REQUEST-MQ-OPEN    VALUE 'O'.                               
              88 WS-REQUEST-MQ-CLSE    VALUE 'C'.                               
           05 WS-REPLY-MQ-FLG          PIC X(01) VALUE 'C'.                     
```
