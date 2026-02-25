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
```
