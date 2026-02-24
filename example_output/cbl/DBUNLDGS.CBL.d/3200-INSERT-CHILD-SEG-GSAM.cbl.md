```cobol
       3200-INSERT-CHILD-SEG-GSAM.                                      02721000
      *     DISPLAY 'IN 3200 INSERT-CHILD-SEG-GSAM'                     02721102
            CALL 'CBLTDLI'       USING  FUNC-ISRT                       02721200
                                        PADFLPCB                        02721300
                                        PENDING-AUTH-DETAILS.           02721400
      *        DISPLAY '***************************'                    02721502
      *        DISPLAY ' AFTER CHILD GSAM IMS CALL'                     02721602
      *        DISPLAY 'PADFL-DBDNAME : ' PADFL-DBDNAME                 02721702
      *        DISPLAY 'PCB STATUS: ' PADFL-PCB-STATUS                  02721802
      *        DISPLAY 'PADFL-PCB-PROCOPT : ' PADFL-PCB-PROCOPT         02721902
      *        DISPLAY '***************************'                    02722002
               IF PADFL-PCB-STATUS NOT EQUAL TO SPACES                  02722101
                  DISPLAY 'GSAM PARENT FAIL :' PADFL-PCB-STATUS         02722201
                  DISPLAY 'KFB AREA IN GSAM:' PADFL-KEYFB               02722301
                  PERFORM 9999-ABEND                                    02722401
               END-IF.                                                  02722501
       3200-EXIT.                                                       02722601
            EXIT.                                                       02723000
```
