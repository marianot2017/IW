*)-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
*)-H ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ Sys.Info ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ
*)-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
*)-H Programa        : IWP862.PRG -> Generacion de archivos ASCII
*)-H
*)-H Release         : 01
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
w_title=upper(prompt())

*) Apertura de archivos--------------------------------------------------------

fieldvec[1]='F01'
fieldvec[2]='F02'
fieldvec[3]='F03'
fieldvec[4]='F04'
fieldvec[5]='F05'
fieldvec[6]='F06'
fieldvec[7]='F12'

use (d0f01) in 0 alias 'f01' order 'd1tip'  &&(Rubros)
use (d0f02) in 0 alias 'f02' order 'd2clie' &&(Clientes)
use (d0f03) in 0 alias 'f03' order 'd3prov' &&(Proveedores)
use (d0f04) in 0 alias 'f04' order 'd4peri' &&(Clientes)
use (d0f05) in 0 alias 'f05' order 'd5pefe' &&(Clientes) Peri Tipo Fech Sucu TiCo TiFo Nume
use (d0f06) in 0 alias 'f06' order 'd6inte' &&(Nombres alternativos)
*use (d0f12) in 0 alias 'f12' EXCLU  		  &&(Parametros de Sistema)	&& R.01 Mariano
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
wcoto		= 'S'

wtrim		= '1'
wano		= right(f04->d4peri,4)

wfivebi		= 'IV'+d0coem+right(wano,2)+wtrim+'.VEN'
wficobi		= 'IV'+d0coem+right(wano,2)+wtrim+'.COM'

w_fivebi	= wfivebi
w_ficobi	= wficobi

wdperi		= ""
whperi		= ""

do ssm998.mpr
do iws862.spr

pop key
pop menu _msysmenu
=closall()
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

if w_vari='WTRIM' or w_show
	if !wtrim$'1234 '
		if !w_show
			wait wind 'Opciones vแlidas: 1,2,3,4'
		endif
		w_retu=.f.
	endif
endif

if w_vari='WANO' or w_show
	if wano<'1985'
		if !w_show
			wait wind 'A๑o no permitido.'
		endif
		w_retu=.f.
	else
		do case
		case wtrim='1'
			wdperi='01'
			whperi='03'
		case wtrim='2'
			wdperi='04'
			whperi='06'
		case wtrim='3'
			wdperi='07'
			whperi='09'
		case wtrim='4'
			wdperi='10'
			whperi='12'
		otherwise
			wdperi='01'
			whperi='12'
		endcase
		
		wdperi=wdperi+'/'+wano
		whperi=whperi+'/'+wano
		
		w_dperi=ptoi(wdperi)
		w_hperi=ptoi(whperi)
		
		wfivebi='IV'+d0coem+right(wano,2)+wtrim+'.VEN'
		wficobi='IV'+d0coem+right(wano,2)+wtrim+'.COM'
		show gets
	endif
endif

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
		w_FIvebi=w_dici+alltrim(wFIvebi)
		w_FIcobi=w_dici+alltrim(wFIcobi)
	endif
endif

if w_vari='WFIVEBI' or w_show
	if empty(wfivebi)
		if !w_show
			wait wind 'Debe especificar el nombre del archivo de ventas.'
		endif
		w_retu=.f.
	endif
endif

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

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PROC Cons862
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
if !valid862(.t.)
	wait wind 'Verifique los datos ingresados...'
	return
endif

if sino('Confirma Generaci๓n ?.')='S'
	wait wind nowait 'Creando Archivo Temporario...'
	if creamov(wDperi,wHperi,'VC','1')
		wait clear
		if !eof('f9c')
		
			dele file &w_FIvebi
			dele file &w_FIcobi
			
			if Sele860(wCoTo)
				sele f9c
				dele tag all
				inde on d5sele+d5tipo+d5cuit+PtoI(d5Peri)+d5tiop+d5coco tag d5sele
				wait wind nowait 'Generando Ventas...'
				=GeAr862('V',w_FIvebi)
				wait wind nowait 'Generando Compras...'
				=GeAr862('C',w_FIcobi)
				wait clear
				=advsuave('Fin del Proceso, Archivos compras,ventas generados.','Informaci๓n')
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

set alternate to &s_file
set console off
set alternate on

sele f9c
if seek('*'+S_tipo,'f9c')

	do while d5sele+d5tipo='*'+S_tipo
		
		stor 0 to w5tota,w5neto
      w5tido=d5tido
      w5cuit=d5cuit
      w5peri=d5peri
      w5tiop=d5tiop
      w5coco=d5coco
      do while d5sele+d5tipo='*'+S_tipo;
         and w5cuit=d5cuit;
         and w5peri=d5peri;
         and w5tiop=d5tiop;
         and w5coco=d5coco
         w5tota = w5tota + d5tota
         w5neto = w5neto + d5neto
         
         skip
         
    	enddo
      ?? left(w5peri,2)+'/'+right(w5peri,2)
      ?? w5tido
      ?? w5cuit
      ?? w5tiop
      ?? w5coco
      ?? w5tota
      ?? w5neto
      ?
	enddo
	
endif
set alternate to
set console on
set alternate off
return('')
