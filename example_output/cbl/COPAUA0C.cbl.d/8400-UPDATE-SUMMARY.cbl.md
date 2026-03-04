```cobol
      * You may obtain a copy of the License at
      *
      *    http://www.apache.org/licenses/LICENSE-2.0
      *
      * Unless required by applicable law or agreed to in writing,
      * software distributed under the License is distributed on an
      * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
      * either express or implied. See the License for the specific
      * language governing permissions and limitations under the License
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. COPAUA0C.                                                    
       AUTHOR.     SOUMA GHOSH.                                                 
                                                                                
       ENVIRONMENT DIVISION.                                                    
       CONFIGURATION SECTION.                                                   
                                                                                
       DATA DIVISION.                                                           
       WORKING-STORAGE SECTION.                                                 
                                                                                
       01 WS-VARIABLES.                                                         
         05 WS-PGM-AUTH                PIC X(08)  VALUE 'COPAUA0C'.             
         05 WS-CICS-TRANID             PIC X(04)  VALUE 'CP00'.                 
         05 WS-ACCTFILENAME            PIC X(8)   VALUE 'ACCTDAT '.             
         05 WS-CUSTFILENAME            PIC X(8)   VALUE 'CUSTDAT '.             
         05 WS-CARDFILENAME            PIC X(8)   VALUE 'CARDDAT '.             
         05 WS-CARDFILENAME-ACCT-PATH  PIC X(8)   VALUE 'CARDAIX '.             
         05 WS-CCXREF-FILE             PIC X(08)  VALUE 'CCXREF  '.             
         05 WS-REQSTS-PROCESS-LIMIT    PIC S9(4)  COMP VALUE 500.               
                                                                                
         05 WS-MSG-PROCESSED           PIC S9(4)  COMP VALUE ZERO.              
         05 WS-REQUEST-QNAME           PIC X(48).                               
         05 WS-REPLY-QNAME             PIC X(48).                               
         05 WS-SAVE-CORRELID           PIC X(24).                               
         05 WS-RESP-LENGTH             PIC S9(4)  VALUE 1.                      
         05 WS-RESP-CD                 PIC S9(09) COMP VALUE ZEROS.             
         05 WS-REAS-CD                 PIC S9(09) COMP VALUE ZEROS.             
                                                                                
         05 WS-ABS-TIME                PIC S9(15) COMP-3 VALUE 0.               
         05 WS-CUR-DATE-X6             PIC X(06)  VALUE SPACES.                 
         05 WS-CUR-TIME-X6             PIC X(06)  VALUE SPACES.                 
         05 WS-CUR-TIME-N6             PIC 9(06)  VALUE ZERO.                   
         05 WS-CUR-TIME-MS             PIC S9(08) COMP.                         
         05 WS-YYDDD                   PIC 9(05).                               
         05 WS-TIME-WITH-MS            PIC S9(09) COMP-3.                       
         05 WS-OPTIONS                 PIC S9(9)  BINARY.                       
         05 WS-COMPCODE                PIC S9(9)  BINARY.                       
         05 WS-REASON                  PIC S9(9)  BINARY.                       
         05 WS-WAIT-INTERVAL           PIC S9(9)  BINARY.                       
         05 WS-CODE-DISPLAY            PIC 9(9).                                
         05 WS-AVAILABLE-AMT           PIC S9(09)V99 COMP-3.                    
```
