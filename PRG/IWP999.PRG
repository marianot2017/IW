*)-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
*)-H ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ Sys.Info ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ
*)-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
*)-H Programa        : IWP999.PRG -> SETEO PRINCIPAL
*)-H Release 		 : 04
*)-H Fecha de inicio : 
*)-H Fecha de fin.   : 
*)-H
*)-H Updates
*)-H
*)-H Fecha       Programmer      Comentarios
*)-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
*)-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
set procedure to library
clear macros


*SET DEVELOP ON


set talk off
set echo off
set date french
set conf on
set bell off
set dele on
set safe off
set excl off
set lock off
set esca off
set print off
set print to
set century on
set resource on
set multilock OFF
set reprocess to 5
set notify off
set autosave on
set bell to 2500,1
set mouse on
set color of message to B*/W
set status bar off
set mackey to
set point to '.'
set separator to ','

publ d0pref

d0pref		= "IW"
w_system	= "IV"

declare VerFiles[100]

w_err1		= 'Bienvenido al Sistema de IVA Compras-Ventas MemoSoft'

w_panta		= "DO " + d0pref + "sver.spr"

w_prog		= "DO " + d0pref + "pver"

n0femp		=	"\" + d0pref + "\" + w_system + "f010"

w_archempr	= "f10"
Fieldvec[1]	= 'F10'

*===> END R10A


=setup()	&& inicializacion de variables de la libreria

=chqnet('IW')

=chqcfg()   && Chequea directorio USERS segun netuser, directorio de TMP y Handles

*) Variables PUBLICas de Lockeo's
PUBLIC d0emp,d0sys,d0coem,d0comp,d0path,d0del,d0vein,d0vees,d0veda &&(Datos de la empresa)
d0sys ='SISTEMA DE IVA'
d0emp =''
d0coem=''
d0Comp=''
d0path=''
d0del =1
d0VeIn=''
d0VeEs=''
d0VeDa=''

PUBLIC d0f98,wmenu2, d0toco, d0pate, d0disc, d0diga
d0f98 ='ssf098'
wmenu2=.f.

d0pate=sys(2023)  && path temporario

if !used('F98')
	f_acce=1
	do while .t.

		Fieldvec[1] = 'F98'
	
		use &d0f98 in 0 alias f98 
	
		if !neterr()
			exit
		else
			if f_acce>=3	&& tres intentos
				=mquit()
   			endif
			f_acce=f_acce+1
		endif

	enddo

endif
sele f98

* Declaraci๓n e inicializacion publica de variables de archivos
do while .not. eof('f98')
   i_file=trim(f98->d98file)
   i_alia=trim(f98->d98alia)
   i_vari='d0'+i_alia

   public &i_vari
   &i_vari=i_file

   if upper(substr(d98alia,4,1))='I'
	   i_vari='d0'+i_alia
   	   public &i_vari.n
   	   * Aqui no se inicializan las variables de indices de IVA
   endif

   skip in f98
enddo

d0f99=d0pate+'\'+left(d0f99,len(d0f99)-3)+p_netuser+right(d0f99,2)

**reindexa archivos de sistema

use ivf025 in 0 alias f25	&& R.01 Mariano

if file('indexar.mem') or !file('ivf010.cdx') or !stag('d25codi', 'f25')		&& R.01 Mariano
	use in f25	&& R.01 Mariano
	go top in f98
	do while !eof('f98')
		if f98.d98sist	&& archivos de sistema
&& R.02b Mariano
			if !EqualStr(allt(f98.d98file)+'.dbf','fi\'+allt(f98.d98file)+'.dbf')	
				skip in f98
				loop
			endif
&& R.02e Mariano
			w_reg98=recno('f98')		
			=db_indexiw(.t., (f98.d98alia))
			go w_reg98 in f98
		endif
		skip in f98
	enddo

endif

&& R.01b Mariano
if used('f25')
	use in f25
endif
&& R.01e Mariano

use in f98
     
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
*) inicializa variables PUBLICas
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
*) Inicializacion de variables que figuran en la funcion setup de 
*) la libreria de clipper

PUBLIC d0memo
d0memo=chr(77)+chr(101)+chr(109)+chr(111)+chr(83)+chr(111)+chr(102)+chr(116)
*)          M       e        m        o        S       o        f        t

*)-----------------------------------------------------------------------------
*) variables sentinel
*) esto es necesario por si no se invoca a la funcion testini
*)-----------------------------------------------------------------------------
if 'U' $ type('w0ses')
   publ w0ses,w0sek,w0mop
   store '' to w0ses,w0sek
   w0mop = ' '
endif

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
*) inicializa variables PUBLICas
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ

=invarpub()

=testini('IW',w_sys,@w_ver,.t.)

set date french
set conf on
set bell off
set dele on
set safe off

PUBLIC wdr,d0del,w0gedi
wdr=sys(5)
w0GeDi=.t.
d0del=1

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
*) Inicializaciขn de variables y vectores para Net.
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
*) File Semaforo
=SetNet('IVF077')

if !login('S')
   clear
   =ErrorNet(001)
   close data
*) set curs on
   =Mquit()
endif

* salidas anormales del sistema
=chqsal()

f_acce=1
do while .t.

	Fieldvec[1] = 'F12'
	
	use in 0 ivf012 alias f12
	if !neterr()
		exit
	else
		if f_acce>=3	&& tres intentos
			=mquit()
		endif
		f_acce=f_acce+1
	endif

enddo
	  
fieldvec=''

sele f12

d0solv=f12->p0solv
d0alia=''

*** parametros de IVA ***

d0disc=p0disc
d0toco=p0toco

*** dispositivo generador de datos
if type('p0diga')='U'
	d0diga=''
else
	d0diga=p0diga
endif

use

if d0toco='S' and !mfile('habi.tst') and !w0mop$'D'
	do iwp010i && Toma datos IVA Dos, activa seleccion de empresas
endif

w_title=space(44)+w_sys+' MEMOSOFTฎ'	&& R.03 Mariano (agregue marca memosoft)

modi wind screen;
   	title w_title ;
  	fill file MemoFond.bmp;
   	font 'arial',10 icon file iw.ico
   	
zoom wind screen max

if !CheckVer()
   clear
	close data
*) set cursor on
   =Mquit()
endif

do while !login('S')
   *)mensa('>> No se pudo abrir <<',23,1)
   wait '>> No se pudo abrir <<' wind time .9
enddo

public w0top
w0top = asc('')   &&  tope de registros de comprobantes 25

w_nivel='1'
wPrimElec=.t.

PUBLIC w0pinu,w0pich,w0nume,wstatus,d0audi

d0audi='N'

w0nume='RV.RC.JU'
w0pich='@!k XXX'
w0pinu='@!k '+repl('9',20)

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
*) Definiciขn de Variables para algunas Opciones de Sistema
*) --------------------------------------------------------
*) Opciones
*) --------
*)    R   > Version Reducida
*)    X   > Reexpresion
*)
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PUBLIC OpcR,OpcX
if 'R'$d0opci
   OpcR=.t.
else
   OpcR=.f.
endif
if 'X'$d0opci
   OpcX  =.t.     
else
   OpcX  =.f.
endif

return

*=====================================================================================
*===> ACTUALIZADOR NUEVO

PROCEDURE checkver

DO dirsafip

wCont=.t.

DO checkver1

RETURN wCont


*===>  NF_VERS = nro. de versi๓n guardada LARGO = 8 !!!!!!!!!!!!!
if (NF_VERS<>wVerxExe .or. mfile('habi.tst') ) .and. set('DEVELOP')='OFF'
   if !TestClav(d0pref)                           &&     Verifica
      return !wCont                               &&
   endif                                          &&     Version
endif                                             &&

wVerxExe	= wVersExe + STRTRAN(wVersRel,' ','') + SUBSTR(w_ver,19,2)

wvercant	= 0
wversdbf	= ""

wVerCant = ADIR(VerFiles,'\' + d0pref + '\ver?????.*')

IF wvercant = 0
			wVerCant = ADIR(VerFiles,'\' + d0pref + '\V???????.*')
ENDIF

w_sort = ASORT(VerFiles)

w_file = Verfiles(1)

IF w_sort = 1	AND wvercant > 0
			IF SUBSTR(verfiles(1),1,3) = "VER"
					   wVersDbf = RIGHT(VerFiles[1,1],3) + SUBS(VerFiles[1,1],4,4) + "  " 
			ELSE
			   wVersDbf = RIGHT(VerFiles[1,1],3) + SUBS(VerFiles[1,1],4,4) + SUBS(VerFiles[1,1],2,2) 
			ENDIF
ELSE
   if mfile('inicial.in')
      * Instalacion Inicial  
      * Crea el VER que corresponde a este EXE

      w_file='\' + d0pref + '\V' + SUBSTR(wVerxExe,8,2) + SUBSTR(wVerxExe,4,4) + '0.' + wVersExe 

      w_hand=f_create(w_file)
         
      if w_hand > 0
					=f_write(w_hand,dtoc(date()) + ' ' + time() + "  ")
					=f_close(w_hand)
      endif

	  wVerCant = ADIR(VerFiles,'\' + d0pref + '\V???????.*')

      wVersDbf = wVerxExe

      dele file inicial.in

      if w0mop=='D'
         w_err2=''
         w_err3='Usted estแ operando en modalidad demostraci๓n'
         w_err4=''
         w_err5='Para acceder a todas las prestaciones deberแ habilitarlo, a trav้s del UTIL'
      
        =AdvSuave(w_err1+chr(13)+w_err2+chr(13)+w_err3+chr(13)+w_err4+chr(13)+w_err5,'')
      endif  

   else
      * Han borrado archivo VER??, se fuerza actualizaci๓n
      	wVersDbf='000'+'00000'
	  	w_file = "V0000000.000"
		w_hand=f_create(w_file)
        if w_hand > 0
				=f_write(w_hand,dtoc(date()) + ' ' + time() + "  ")
				=f_close(w_hand)
		ELSE
			=advgrave("ERROR : NO EXISTE VERSION"+CHR(13)+CHR(13)+"COMUNICARSE CON MEMOSOFT")
			RETURN .F.
		endif
   endif   
ENDIF

* EMPRESAS SIN ACTUALIZAR. Control Empresa por Empresa
* ฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿

wEmSinAct=0

f_acce=1
do while .t.
	
    use &n0femp in 0 alias f10
    
    if !neterr()
        exit
	else
		if f_acce>=3	&& tres intentos
			=mquit()
		endif
		f_acce=f_acce+1
    endif
    
enddo

*if type('f10.p0VeDa')='C'
if type(w_archempr + ".p0veda")	=	'C'

   do while !eof('f10')

      if f10->p0VeEs+f10->p0VeDa <> wVersExe+wVersExe
         wEmSinAct=wEmSinAct+1
      endif
      skip in f10
   enddo
   if wEmSinAct>0
   
   	   if wemsinact=1
	       w_err1='ATENCION!! : FUE ENCONTRADA UNA EMPRESA DESACTUALIZADA'
	       =advsuave(w_err1)
	   else
	       w_err1='ATENCION!! : FUERON ENCONTRADAS '+ltrim(str(wEmSinAct))+' EMPRESAS DESACTUALIZADAS'
	       =advgrave(w_err1)
	   endif

   endif
endif

sele f10
use

w_a	= " "

IF wvercant > 0		AND SUBSTR(w_file,1,3) <> "VER"
			w_hand	= FOPEN(w_file)
			IF w_hand < 0
						=advgrave("ERROR DE ACTUALIZACION"+CHR(13)+CHR(13)+"COMUNICARSE CON MEMOSOFT")
						RETURN .F.
			ENDIF
			w_b		= FREAD(w_hand,verfiles(1,2))
			w_a		= SUBSTR(w_b,verfiles(1,2),1)
			=FCLOSE(w_hand)
ENDIF

DO CASE
*	 			Versi๓n > Instalada
		CASE SUBSTR(wVerxExe,1,7) > SUBSTR(wVersDbf,1,7) 
				w_actver = .T.

*				Versi๓n y Release = Instalado, Versi๓n termina en "A" e instalado NO
		CASE w_rel = "A"	AND w_rel <> w_a	AND wVerxExe = wVersDbf		 
				w_actver = .T.

*				Release > Instalado y termina en "A"
		CASE SUBSTR(wVerxExe,8,2) > SUBSTR(wVersDbf,8,2)	AND w_rel = "A"		 
				w_actver = .T.

*				Release > Instalado y NO termina en "A"		===> Busco si hay alg๚n Release con A entre ambos Releases
		CASE SUBSTR(wVerxExe,8,2) > SUBSTR(wVersDbf,8,2)	AND w_rel = "0"
				w_ind2 = VAL(SUBSTR(wVerxExe,8,2)) + 1
				w_ind1 = VAL(SUBSTR(wVersDbf,8,2)) + 2
				FOR n = w_ind1 TO w_ind2
						IF w_veract(n)	= "A"
										w_actver = .T.
										n = 1000
						ENDIF
				ENDFOR
ENDCASE

IF	w_actver	OR wEmSinAct > 0
			wEjec='N'

			&w_panta

			if wEjec='S'
				if ! mfile("\" + d0pref + "\fi\" + w_system + SUBSTR(w_archempr,1,1) + "0" + SUBSTR(w_archempr,2, LEN(w_archempr) - 1) + ".dbf")
						=advgrave("NO EXISTE DIRECTORIO DE ESTRUCTURAS"+CHR(13)+CHR(13)+"COMUNICARSE CON MEMOSOFT")
						RETURN .F.
				else

					&w_prog
         
					dele file &w_File
					for i=1 to wVerCant
							w_File=VerFiles[i,1]
							dele file &w_File
					next

					w_file='\' + d0pref + '\V'+ SUBSTR(wverxexe, 8,12) + SUBSTR(wVerxExe,4,4)+'0.'+wVersExe 

					w_hand=f_create(w_file)
         
					if w_hand > 0
							IF w_rel = "A"
									=f_write(w_hand,dtoc(date()) + ' ' + time() + " A")
									=f_close(w_hand)
							ELSE
								=f_write(w_hand,dtoc(date()) + ' ' + time() + "  ")
								=f_close(w_hand)
							ENDIF
					ELSE
						=advgrave("ERROR AL GENERAR ACTUALIZACION"+CHR(13)+CHR(13)+"COMUNICARSE CON MEMOSOFT")
						RETURN .F.
					endif

					IF w_leer
							w_resul	=	sino("Desea ver la documentaci๓n de esta versi๓n ?","SI", "Actualizaci๓n de versi๓n")
							IF w_resul = "S"
										DO ap WITH d0pref + "p096"
							ENDIF
					ENDIF
        		endif
			endif
ELSE
	IF wverxexe < wVersDbf
					=advgrave("ESTA INTENTANDO INSTALAR :" + CHR(13) + CHR(13) + w_ver + CHR(13) + CHR(13) + "QUE ES ANTERIOR A LA EXISTENTE.")
					RETURN .F.			
	ENDIF
	DELE FILE &verfiles(1,1)
	verfiles(1,1) = SUBSTR(verfiles(1,1),1,1) + SUBSTR(wverxexe,8,2) + SUBSTR(verfiles(1,1),4,LEN(verfiles(1,1))-3)
	w_hand	= FCREATE(verfiles(1,1))
	IF w_hand > 0
			  =FWRITE(w_hand,dtoc(date()) + ' ' + time() + " " + w_rel)
			  =FCLOSE(w_hand)
	ENDIF
ENDIF

close data

RETURN wCont

*=====================================================================================
PROCEDURE dirsafip

IF !mfile('\' + d0pref +'\ARCHIVOS\NUL')
			=creadir('\' + d0pref +'\ARCHIVOS')
ENDIF

IF !mfile('\' + d0pref +'\ARCHIVOS\SDIARIOS\NUL')
			=creadir('\' + d0pref +'\ARCHIVOS\SDIARIOS')
ENDIF

IF !mfile('\' + d0pref +'\AFIP\NUL')
			=creadir('\' + d0pref +'\AFIP')
ENDIF

IF !mfile('\' + d0pref +'\AFIP\IVA\NUL')
			=creadir('\' + d0pref +'\AFIP\IVA')
ENDIF

IF !mfile('\' + d0pref +'\AFIP\SICORE\NUL')
			=creadir('\' + d0pref +'\AFIP\SICORE')
ENDIF

IF !mfile('\' + d0pref +'\AFIP\SIFERE\NUL')
			=creadir('\' + d0pref +'\AFIP\SIFERE')
ENDIF

IF !mfile('\' + d0pref +'\AFIP\CITI\NUL')
			=creadir('\' + d0pref +'\AFIP\CITI')
ENDIF
&& R.04b Mariano
IF !mfile('\' + d0pref +'\AFIP\REGINFO\NUL')
			=creadir('\' + d0pref +'\AFIP\REGINFO')
ENDIF
&& R.04e Mariano
IF !mfile('\' + d0pref +'\AFIP\INGBRU\NUL')
			=creadir('\' + d0pref +'\AFIP\INGBRU')
ENDIF

IF !mfile('\' + d0pref +'\AFIP\INGBRU\CAPITAL\NUL')
			=creadir('\' + d0pref +'\AFIP\INGBRU\CAPITAL')
ENDIF

IF !mfile('\' + d0pref +'\AFIP\INGBRU\BAIRES\NUL')
			=creadir('\' + d0pref +'\AFIP\INGBRU\BAIRES')
ENDIF

RETURN
