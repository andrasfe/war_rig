```cobol
       PROCESS-PAGE-FORWARD.
      *****************************************************************

           IF ERR-FLG-OFF

               MOVE 1             TO  WS-IDX

               MOVE LOW-VALUES    TO CDEMO-CPVS-PAUKEY-LAST

               PERFORM UNTIL WS-IDX > 5 OR AUTHS-EOF OR ERR-FLG-ON
                   IF EIBAID = DFHPF7 AND WS-IDX = 1
                      PERFORM REPOSITION-AUTHORIZATIONS
                   ELSE
                      PERFORM GET-AUTHORIZATIONS
                   END-IF
                   IF AUTHS-NOT-EOF AND ERR-FLG-OFF
                       PERFORM POPULATE-AUTH-LIST
                       COMPUTE WS-IDX = WS-IDX + 1

                       MOVE PA-AUTHORIZATION-KEY TO
                                             CDEMO-CPVS-PAUKEY-LAST
                       IF WS-IDX = 2
                          COMPUTE CDEMO-CPVS-PAGE-NUM =
                                  CDEMO-CPVS-PAGE-NUM + 1
                          MOVE PA-AUTHORIZATION-KEY TO
                          CDEMO-CPVS-PAUKEY-PREV-PG(CDEMO-CPVS-PAGE-NUM)
                       END-IF
                   END-IF
               END-PERFORM

               IF AUTHS-NOT-EOF AND ERR-FLG-OFF
                   PERFORM GET-AUTHORIZATIONS
                   IF AUTHS-NOT-EOF AND ERR-FLG-OFF
                       SET NEXT-PAGE-YES TO TRUE
                   ELSE
                       SET NEXT-PAGE-NO TO TRUE
                   END-IF
               END-IF

           END-IF.

```
