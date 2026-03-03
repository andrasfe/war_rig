```cobol
       3100-INSERT-PARENT-SEG-GSAM.                                     02710200
      *     DISPLAY 'IN 3100 INSERT-PARENT-SEG-GSAM'                    02710302
            CALL 'CBLTDLI'       USING  FUNC-ISRT                       02710400
                                        PASFLPCB                        02710500
                                        PENDING-AUTH-SUMMARY.           02710600
      *        DISPLAY '***************************'                    02710802
      *        DISPLAY ' AFTER PARENT GSAM IMS CALL'                    02710902
      *        DISPLAY ' PASFL-DBDNAME : ' PASFL-DBDNAME                02711002
      *        DISPLAY ' PASFL-PCB-PROCOPT : ' PASFL-PCB-PROCOPT        02711102
      *        DISPLAY 'PCB STATUS: ' PASFL-PCB-STATUS                  02711202
      *        DISPLAY '***************************'                    02711302
               IF PASFL-PCB-STATUS NOT EQUAL TO SPACES                  02711401
                  DISPLAY 'GSAM PARENT FAIL :' PASFL-PCB-STATUS         02711501
                  DISPLAY 'KFB AREA IN GSAM:' PASFL-KEYFB               02711601
                  PERFORM 9999-ABEND                                    02711701
               END-IF.                                                  02711801
       3100-EXIT.                                                       02712000
            EXIT.                                                       02713000
      *----------------------------------------------------------------*02720000
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
