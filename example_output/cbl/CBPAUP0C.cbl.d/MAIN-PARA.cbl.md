```cobol
      *----------------------------------------------------------------*
       3000-FIND-NEXT-AUTH-DTL.
      *----------------------------------------------------------------*
      *
            IF DEBUG-ON
               DISPLAY 'DEBUG: AUTH DTL READ : ' WS-NO-DTL-READ
            END-IF

            EXEC DLI GNP USING PCB(PAUT-PCB-NUM)
                 SEGMENT (PAUTDTL1)
                 INTO (PENDING-AUTH-DETAILS)
            END-EXEC
            EVALUATE DIBSTAT
               WHEN '  '
                    SET MORE-AUTHS       TO TRUE
                    ADD 1                TO WS-NO-DTL-READ
```
