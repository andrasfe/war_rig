       MAIN-PARA.                                                               
                                                                                
           PERFORM 1000-INITIALIZE    THRU 1000-EXIT                            
           PERFORM 2000-MAIN-PROCESS  THRU 2000-EXIT                            
           PERFORM 9000-TERMINATE     THRU 9000-EXIT                            
                                                                                
           EXEC CICS RETURN                                                     
           END-EXEC.                                                            
