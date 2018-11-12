*)-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
*)-H ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ Sys.Info ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ
*)-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
*)-H Programa        : IWP862.PRG -> Generacion de archivos CITI COMPRAS
*)-H
*)-H Release         : 04
*)-H Fecha de inicio : 22/12/1998.-
*)-H Fecha de fin    : 29/12/1998.-
*)-H
*)-H Actualizaciones
*)-H
*)-H Fecha       Programmer      Comentarios
*)-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
*)-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PRIVATE ALL LIKE w_*

*) Salvado de estados anteriores-----------------------------------------------

push key clear		  &&(Salva estados de teclas)
push menu _msysmenu &&(Salva estado de menu)
set skip of menu _msysmenu .t.

*) Variables internas----------------------------------------------------------

w_prog=upper(prompt())
w_alta=.f.
w_pfile='862'
*w_title=upper(prompt())	&& R.03 Mariano
w_title='SIAP - CITI - '+upper(prompt())	&& R.03 Mariano

*) Apertura de archivos--------------------------------------------------------

fieldvec[1]='F00'	&& R.01 Ra๚l 
fieldvec[2]='F01'
fieldvec[3]='F02'
fieldvec[4]='F03'
fieldvec[5]='F04'
fieldvec[6]='F05'
fieldvec[7]='F06'
fieldvec[8]='F12'

use (d0f00) in 0 alias 'f00'												&& R.01 Ra๚l 
use (d0f01) in 0 alias 'f01' order 'd1tip'  &&(Rubros)
use (d0f02) in 0 alias 'f02' order 'd2clie' &&(Clientes)
use (d0f03) in 0 alias 'f03' order 'd3prov' &&(Proveedores)
use (d0f04) in 0 alias 'f04' order 'd4peri' &&(Clientes)
use (d0f05) in 0 alias 'f05' order 'd5pefe' &&(Clientes) Peri Tipo Fech Sucu TiCo TiFo Nume
use (d0f06) in 0 alias 'f06' order 'd6inte' &&(Nombres alternativos)
*use (d0f12) in 0 alias 'f12' EXCLU    		&&(Parametros de Sistema)	&& R.01 Mariano
use (d0f12) in 0 alias 'f12' 		  		&&(Parametros de Sistema)	&& R.01 Mariano


if neterr()
	pop key
	pop menu _msysmenu
	fieldvec=''
	=closall()
	return
endif
*)-----------------------------------------------------------------------------

=SetComp() &&(Setea filtros de comprobante DB CR y su signo)

go bott in f04
go top in f12

wdici		= f12.p0dici
wcoto		= 'N'

wtrim		= '1'
wano		= right(f04->d4peri,4)

&& R.01b Ra๚l
*wdperi		= ""
*whperi		= ""
wdperi		= f04.d4peri
whperi		= f04.d4peri

*wfivebi		= 'IV'+d0coem+right(wano,2)+wtrim+'.VEN'
*wficobi		= 'IV'+d0coem+right(wano,2)+wtrim+'.COM'
wficobi			= 'C'+d0coem+right(wdperi,2)+left(wdperi,2)+'.TXT'

*w_fivebi	= wfivebi
&& R.01e Ra๚l

w_ficobi	= wficobi

do ssm998.mpr
do iws862.spr

pop key
pop menu _msysmenu
=closall()
DELE FILE &d0f99.c.dbf
return

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
FUNC valid862
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PARAMETERS w_show

if empty(parameters())
	w_show=.f.
endif

PRIVATE w_retu,w_vari
w_retu=.t.
w_vari=varread()

&& R.01b Ta๚l

if w_vari='WDPERI' or w_show
	if lastkey()=p_f4
		do fhelp with 'WDPERI','','iwp004','f04','d4peri','d4peri','',(p_char),'w_retu'
	endif
	w_fech	= CTOD('01/' + wdperi)
	IF EMPTY(w_fech)
			RETURN	.F. 
	ENDIF				
	IF TRDATE('w_fech')
			wdperi	= RIGHT(DTOC(w_fech),7)
			SHOW GET wdperi
	ELSE
	    IF !w_show
			WAIT WIND "El perํodo no es correcto"
	    ENDIF
	    w_retu	= .F.
	    _curobj	= objnum(wdperi)
	    RETURN .F.
	ENDIF
	wkperi = ptoi(ALLT(wdperi))
	if ! seek(wkperi,'f04')
			if !w_show
				wait window 'Perํodo Inexistente'
			endif
			w_retu=.f.
	endif

	IF w_retu
			wficobi	= 'C'+d0coem+right(wdperi,2)+left(wdperi,2)+'.TXT'
			whperi		= wdperi
	ELSE
		wficobi	= "ERROR.TXT"
	ENDIF
	SHOW GETS
endif

*if w_vari='WTRIM' or w_show
*	if !wtrim$'1234 '
*		if !w_show
*			wait wind 'Opciones vแlidas: 1,2,3,4'
*		endif
*		w_retu=.f.
*	endif
*endif

*if w_vari='WANO' or w_show
*	if wano<'1985'
*		if !w_show
*			wait wind 'A๑o no permitido.'
*		endif
*		w_retu=.f.
*	else
*		do case
*		case wtrim='1'
*			wdperi='01'
*			whperi='03'
*		case wtrim='2'
*			wdperi='04'
*			whperi='06'
*		case wtrim='3'
*			wdperi='07'
*			whperi='09'
*		case wtrim='4'
*			wdperi='10'
*			whperi='12'
*		otherwise
*			wdperi='01'
*			whperi='12'
*		endcase
		
*		wdperi=wdperi+'/'+wano
*		whperi=whperi+'/'+wano
		
		w_dperi=ptoi(wdperi)
		w_hperi=ptoi(whperi)
		
*		wfivebi='IV'+d0coem+right(wano,2)+wtrim+'.VEN'
*		wficobi='IV'+d0coem+right(wano,2)+wtrim+'.TXT'
		show gets
*	endif
*endif

if wdperi>whperi
	w_retu=.f.
endif

if w_vari='WDICI' or w_show
	w_DiCi=alltrim(wDiCi)
	wDiCi =subs(iif(subs(w_DiCi,len(w_DiCi),1)<>'\',w_DiCi+'\',w_DiCi)+spac(20),1,20)
	w_DiCi=alltrim(wDiCi)	
	if !mfile(w_DiCi+'NUL')
		if sino('Desea crear el directorio -> '+w_dici)='S'
			if mkdir(left(w_dici,len(w_dici)-1)) = 0
	  			=advsuave('Creo el directorio -> '+w_dici)
			else
				=advgrave('No pudo crear el directorio -> '+w_dici)
				w_retu=.f.
			endif
		else
			=advgrave('No existe el directorio -> '+w_dici)
			w_retu=.f.
		endif
	else
*		w_FIvebi=w_dici+alltrim(wFIvebi)			&& R.01 Ra๚l
		w_FIcobi=w_dici+alltrim(wFIcobi)
	endif
endif

&& R.01b Ra๚l
*if w_vari='WFIVEBI' or w_show
*	if empty(wfivebi)
*		if !w_show
*			wait wind 'Debe especificar el nombre del archivo de ventas.'
*		endif
*		w_retu=.f.
*	endif
*endif
&& R.01e Ra๚l

if w_vari='WFICOBI' or w_show
	if empty(wficobi)
		if !w_show
			wait wind 'Debe especificar el nombre del archivo de compras.'
		endif
		w_retu=.f.
	endif
endif

if w_vari='WCOTO' or w_show
	if !wcoto$'SN'
		if !w_show
			wait wind 'Opciones vแlidas: S,N'
		endif
		w_retu=.f.
	endif
endif
return(w_retu)

***vfp***
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
proc cons862
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
	on key label f9 do cons862x
	keyboard '{f9}'
return

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
proc cons862x
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
on key label f9
***vfp***

if !valid862(.t.)
	wait wind 'Verifique los datos ingresados...'
	return
endif

if sino('Confirma Generaci๓n ?.')='S'
	wait wind nowait 'Creando Archivo Temporario...'
	
	if creamov(wDperi,wHperi,'VC','1')
		wait clear
		if !eof('f9c')
		
*			dele file &w_FIvebi	&& R.02 Mariano
			dele file &w_FIcobi
			
			if Sele860(wCoTo)
				sele f9c

*				DELE ALL FOR d5tipo <> 'C'								&& R.01 Ra๚l	&& R.02 Mariano
				DELE ALL FOR d5tipo <> 'C'	and d5tico!='C'				&& R.02 Mariano	(Elimino ventas que no sean NC)
				GO TOP
				DELE ALL FOR d5tipo = 'C'	AND d5tico = 'R'			&& R.01 Ra๚l	(Elimino retenciones)

*				dele for !d5coco$'01.02.03.04.05.08.13.30.34.37.39.42.50.51.52.53.54.55.56.57.58.59.60.63.70.81.84.86'	&& R.02 Mariano	&& R.03 Mariano
				dele for !d5coco$'001.002.003.004.005.008.013.030.034.037.039.042.050.051.052.053.054.055.056.057.058.059.060.063.070.081.084.086.110.112.113.114.115.118.119.120'	&& R.02 Mariano	&& R.03 Mariano

				dele tag all
				inde on d5sele+d5tipo+d5cuit+PtoI(d5Peri)+d5tiop+d5coco tag d5sele
&& R.01b Ra๚l
*				wait wind nowait 'Generando Ventas...'
*				=GeAr862('V',w_FIvebi)
&& R.01e Ra๚l
				wait wind nowait 'Generando Compras...'
				=GeAr862('C',w_FIcobi)
				wait clear
*				=advsuave('Fin del Proceso, Archivos compras,ventas generados.','Informaci๓n')
				=advsuave('Fin del Proceso, Archivo compras generado.','Informaci๓n')
			else
				=advsuave('No Hay datos que Cumplan...','Informaci๓n')
			endif
			
		else
			=advsuave('No hay datos...','Informaci๓n')
		endif
		sele f9c
		use
		dele file &d0f99.c.dbf
		dele file &d0f99.c.cdx
	endif
	wait clear
endif
return

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
FUNC gear862
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PARAMETERS s_tipo,s_file

IF USED('f99d')
		USE in f99d		&& R.03 Mariano (agregue in f99d)
		DELE FILE &d0f99.d.dbf
ENDIF		

SELE 0

CREATE dbf &d0f99.d (d99reng C(254), d99line L)
USE

USE &d0f99.d IN 0 ALIAS f99d EXCL

set alternate to &s_file
set console off
set alternate on

sele f9c
*if seek('*'+S_tipo,'f9c')	&& R.02 Mariano
if start('*'+S_tipo,'f9c')	&& R.02 Mariano

	do while d5sele+d5tipo='*'+S_tipo or d5sele+d5tipo='*V'
		
	  stor 0 to w5tota,w5neto,w5impu	&& R.02 Mariano (agregue w5impu)
	  
      w5tido=d5tido
      w5cuit=d5cuit

      w5peri=d5peri
      w5tiop=d5tiop
	  w5coco=d5coco
	  
&& R.01b Ra๚l 
		w5nume	= d5nume	     
		w5fech	= d5fech   
*		w5nocl	= d5nocl	&& R.02 Mariano
		w5clie	= d5clie	  	  

*      do while d5sele+d5tipo='*'+S_tipo;
*         and w5cuit=d5cuit;
*         and w5peri=d5peri;
*         and w5tiop=d5tiop;
*         and w5coco=d5coco
         
      do while (d5sele+d5tipo='*'+S_tipo or d5sele+d5tipo='*V');
         and w5clie=d5clie;
         and w5peri=d5peri;
         and w5tiop=d5tiop;
         and w5coco=d5coco;
         AND w5nume	= d5nume	&& R.02 Mariano (incluye NC ventas)
&& R.01e Ra๚l
         
         w5tota = w5tota + d5tota
         w5neto = w5neto + d5neto
		 w5impu = w5impu + d5imp4+d5imp5	&& R.02 Mariano
         skip
         
      enddo

      && R.02b Mariano
	  skip -1 
	  if d5tipo='V'
	  	  w5tido='80'
	  	  w5cuit=f00.p0cui
	  	  w5nocl=left(f00.p0emp,25)
	  	  w5nume=d5sucu+'-'+padl(allt(strt(w5nume,'/','')),8,'0')
	  else
	      w5tido=d5tido
    	  w5cuit=d5cuit
    	  w5nocl=d5nocl
*	  	  if w5coco$'03.08.13.53'	&& R.03 Mariano
	  	  if w5coco$'003.008.013.053.110.112.113.114.119'	&& R.03 Mariano
		  	w5coco='42'
		  && R.03b Mariano
		  else
		  	if val(w5coco)<=99
		  		w5coco=right(w5coco,2)
		  	else
		  		do case 
		  			case w5coco='115'
		  				w5coco='02'
		  			case w5coco='118'
		  				w5coco='51'
		  			case w5coco='120'
		  				w5coco='52'
		  		endcase
		  	endif
		  && R.03e Mariano
		  endif
      endif
      skip
      && R.02e Mariano
	  
&& R.01b Ra๚l
*      ?? left(w5peri,2)+'/'+right(w5peri,2)
*      ?? w5tido
*      ?? w5cuit
*      ?? w5tiop
*      ?? w5coco
*      ?? w5tota
*      ?? w5neto
*      ?
		if w5impu!=0	&& R.02 Mariano
			w5neto	=	STRTRAN(STR(w5neto,12,2),' ','0')
			w5neto	=	STRTRAN(w5neto,'.','')
			w5neto	=	'0' + w5neto

			?? w5coco

			&& R.02b Mariano
*			?? LEFT(w5nume,4)
*			?? '000000000000'
*			?? RIGHT(w5nume,8)
*			?? left(strt(strt(w5nume,'-',''),'/',''),4)+'000000000000'+right(strt(strt(w5nume,'-',''),'/',''),8)	&& R.04 Mariano
			?? left(strt(strt(w5nume,'-',''),'/',''),4)+'000000000000'+right(rtrim(strt(strt(w5nume,'-',''),'/','')),8)	&& R.04 Mariano
			&& R.02e Mariano

			?? STRTRAN(DTOC(w5fech), '/', '')
			?? STRTRAN(w5cuit, '-', '')	
			?? LEFT(w5nocl, 25)
*			?? w5neto	&& R.02 Mariano
			?? strcero(right(str(abs(w5impu)*100,13),12))	&& R.02 Mariano
*			?? '           '	&& R.02 Mariano
			?? '00000000000'	&& R.02 Mariano

			?? SPACE(25)

*			?? '            '	&& R.02 Mariano
			?? '000000000000'	&& R.02 Mariano
 			?

*			w_99reng = w5coco + ' ' + LEFT(w5nume,4) + '000000000000' + RIGHT(w5nume,8) + ' ' + DTOC(w5fech) + ' ' + w5cuit + ' ' + LEFT(w5nocl, 25) + ' ' + w5neto	&& R.02 Mariano
*			w_99reng = w5coco + ' ' + left(strt(strt(w5nume,'-',''),'/',''),4)+'000000000000'+right(strt(strt(w5nume,'-',''),'/',''),8) + ' ' + DTOC(w5fech) + ' ' + w5cuit + ' ' + LEFT(w5nocl, 25) + ' ' + str(abs(w5impu),12,2)	&& R.02 Mariano	&& R.04 Mariano
			w_99reng = w5coco + ' ' + left(strt(strt(w5nume,'-',''),'/',''),4)+'000000000000'+right(rtrim(strt(strt(w5nume,'-',''),'/','')),8) + ' ' + DTOC(w5fech) + ' ' + w5cuit + ' ' + LEFT(w5nocl, 25) + ' ' + str(abs(w5impu),12,2)	&& R.04 Mariano
			SELE f99d
			APPE BLAN
			REPLA d99reng		WITH w_99reng
			SELE f9c
		endif	&& R.02 Mariano


&& R.01e Ra๚l
	enddo
	
endif
set alternate to
set console on
set alternate off

if !p_cancela
	return
endif

do repdest &&(Setea variables internas del reporte)

sele f99d
go top
if !empty(p_repclaus) &&(debe ser preview o to printer)
	p_report='iwr862'
	=rep_form()
	p_report=''
endif

do represet &&(Resetea estado)

return('')


***puesto adrede para que incluya el report al proyecto**
report form iwr862
