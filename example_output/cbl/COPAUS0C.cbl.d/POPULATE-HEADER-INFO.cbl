              88 SEND-ERASE-YES                    VALUE 'Y'.                   
              88 SEND-ERASE-NO                     VALUE 'N'.                   
                                                                                
       COPY COCOM01Y.                                                           
          05 CDEMO-CPVS-INFO.                                                   
             10 CDEMO-CPVS-PAU-SEL-FLG     PIC X(01).                           
             10 CDEMO-CPVS-PAU-SELECTED    PIC X(08).                           
             10 CDEMO-CPVS-PAUKEY-PREV-PG  PIC X(08) OCCURS 20 TIMES.           
             10 CDEMO-CPVS-PAUKEY-LAST     PIC X(08).                           
             10 CDEMO-CPVS-PAGE-NUM        PIC S9(04) COMP.                     
             10 CDEMO-CPVS-NEXT-PAGE-FLG   PIC X(01) VALUE 'N'.                 
                88 NEXT-PAGE-YES                     VALUE 'Y'.                 
                88 NEXT-PAGE-NO                      VALUE 'N'.                 
             10 CDEMO-CPVS-AUTH-KEYS       PIC X(08) OCCURS 5 TIMES.            
                                                                                
      *BMS Copybook
       COPY COPAU00.

      *Screen Titles
       COPY COTTL01Y.

      *Current Date
