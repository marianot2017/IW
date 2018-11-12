*)-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
*)-H ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ Sys.Info ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ
*)-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
*)-H Programa        : IWP866.PRG -> Generacion de archivos ASCII - Mensual
*)-H
*)-H Fecha de inicio :
*)-H Fecha de fin    :
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
w_pfile='866'
w_title=upper(prompt())

*) Apertura de archivos--------------------------------------------------------

fieldvec[1]='F01'
fieldvec[2]='F02'
fieldvec[3]='F03'
fieldvec[4]='F04'
fieldvec[5]='F05'
fieldvec[6]='F06'
fieldvec[7]='F12'
fieldvec[8]='F23'

use (d0f01) in 0 alias 'f01' order 'd1tip'  &&(Rubros)
use (d0f02) in 0 alias 'f02' order 'd2clie' &&(Clientes)
use (d0f03) in 0 alias 'f03' order 'd3prov' &&(Proveedores)
use (d0f04) in 0 alias 'f04' order 'd4peri' &&(Clientes)
use (d0f05) in 0 alias 'f05' order 'd5pefe' &&(Clientes) Peri Tipo Fech Sucu TiCo TiFo Nume
use (d0f06) in 0 alias 'f06' order 'd6inte' &&(Nombres alternativos)
use (d0f12) in 0 alias 'f12' EXCLU  		  &&(Parametros de Sistema)
use (d0f23) in 0 alias 'f23' order 'd23codi' &&(Alicuotas IVA)

*)-----------------------------------------------------------------------------

=SetComp() &&(Setea filtros de comprobante DB CR y su signo)

go bott in f04
go top in f12

wtrim=''
wdici=f12->p0dici
wcoto='N'
store '' to wfivebi
store '' to w_fivebi

WDPERI=F04.D4PERI
WHPERI=F04.D4PERI

do ssm998.mpr
do iws866.spr

pop key
pop menu _msysmenu
=closall()
return

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
FUNC valid866
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PARAMETERS w_show

if empty(parameters())
	w_show=.f.
endif

PRIVATE w_retu,w_vari
w_retu=.t.
w_vari=varread()

if w_vari='WDPERI' or w_show
	if lastkey()=p_f4
		do fhelp with 'WDPERI','','iwp004','f04','d4peri','d4peri','',(p_char),'w_retu'
	endif
endif

if w_vari='WHPERI' or w_show
	if lastkey()=p_f4
		do fhelp with 'WHPERI','','iwp004','f04','d4peri','d4peri','',(p_char),'w_retu'
	endif

	wfivebi='IV'+d0coem+right(WDPERI,2)+'M'+'.TXT'

endif

if WDPERI>WHPERI
	if !w_show
		wait wind 'Hasta debe ser mayor o igual que desde...'
	endif
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
PROC Cons866
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
if !valid866(.t.)
	wait wind 'Verifique los datos ingresados...'
	return
endif

if sino('Confirma Generaci๓n ?.')='S'
	wait wind nowait 'Creando Archivo Temporario...'
	if creamov(wDperi,wHperi,'V','1',' ',.t.)
		wait clear
		if !eof('f9c')

			dele file &w_FIvebi

			if Sele860(wCoTo)
				sele f9c
				dele tag all
				inde on d5sele+d5tipo+d5cuit+PtoI(d5Peri)+d5tiop+d5coco tag d5sele
				wait wind nowait 'Generando Ventas...'
				=GeAr866('V',w_FIvebi)
				wait clear
				=advsuave('Fin del Proceso, Archivos generados.','Informaci๓n')
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
FUNC gear866
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PARAMETERS s_tipo,s_file

set alternate to &s_file
set console off
set alternate on

sele f9c
if seek('*'+S_tipo,'f9c')

	store 0 to w5tota,w5neto,w5caal,w5otros,w5exen,w5impu,w5imre

	do while d5sele+d5tipo='*'+S_tipo

*set step on
*acti wind debug


		w5peri=d5peri
		w5coco=d5coco
		w5nume=d5nume

		w5nocl=left(iif(d5clie>='99990', iif(seek(d5inte,'f06'), f06.d6nocl, 'No encontrado!!!'), iif(seek(d5clie,'f02'), f02.d2nomb, 'No encontrado!!!')),30)
		w5tido=iif(d5clie>='99990', iif(seek(d5inte,'f06'), f06.d6tido, '  '), iif(seek(d5clie,'f02'), f02.d2tido, '  '))

		if d5tifo='B' and w5tido='99' and d5tota < 1000
			w5nuic='00000000000'
		else
			w5nuic=left(d5cuit,2)+substr(d5cuit,4,8)+right(d5cuit,1)
		endif

		w5alic=iif(seek(d5alic,'F23'),strcero(str(f23.d23alic*100,4)),'0000')

		w5tota = w5tota + d5tota
		w5neto = w5neto + d5neto
		w5otros= w5otros + d5imp6 + d5imp7 + d5imp8
		w5exen= w5exen + iif((d5imp2=d5tota),d5imp2,0)
		w5impu= w5impu + d5imp3 + d5imp4
		
		if d5tifo='M'
			w5imre=w5imre+d5imre
		endif
		
		w5caal = w5caal + 1
		
		skip

		if d5sele+d5tipo='*'+S_tipo;
				and w5peri=d5peri;
				and w5coco=d5coco;
				and w5nume=d5nume

			if iif(seek(d5alic,'F23'),strcero(str(f23.d23alic*100,4)),'0000')<>w5alic
				w5caal = w5caal + 1
			endif

			skip -1

			skip

			if d5sele+d5tipo='*'+S_tipo;
					and w5peri=d5peri;
					and w5coco=d5coco;
					and w5nume=d5nume

				skip -1

				?? '1'
				?? dtos(max(ctod('01/'+d5peri),d5fech))
				?? d5coco
				?? space(1)
				?? d5sucu
				?? '000000000000'+left(d5nume,8)
				?? '000000000000'+left(d5nume,8)
				?? w5tido
				?? w5nuic
				?? w5nocl
				?? '000000000000000'
				?? '000000000000000'
				?? '000000000000000'
				?? w5alic
				?? '000000000000000'
				?? '000000000000000'
				?? '000000000000000'
				?? '000000000000000'
				?? '000000000000000'
				?? '000000000000000'
				?? '000000000000000'
				?? '00'
				?? space(3)
				?? '0000000000'
				?? str(w5caal,1)
				?? space(1)
				?? '00000000000000'
				?? '00000000'
				?? '00000000'
				?? space(75)
				?? '00000000'
				?? '000000000000000'
				?

				skip

			else

				skip - 1

				?? '1'
				?? dtos(max(ctod('01/'+d5peri),d5fech))
				?? d5coco
				?? space(1)
				?? d5sucu
				?? '000000000000'+left(d5nume,8)
				?? '000000000000'+left(d5nume,8)
				?? w5tido
				?? w5nuic
				?? w5nocl
				?? strcero(str((w5tota*100),15))
				?? strcero(str(((w5otros)*100),15))
				?? strcero(str((w5neto*100),15))
				?? w5alic
				?? strcero(str(((w5impu)*100),15))
				?? '000000000000000'
				?? strcero(str((w5exen*100),15))
				?? '000000000000000'
				?? '000000000000000'
				?? '000000000000000'
				?? '000000000000000'
				?? '00'
				?? space(3)
				?? '0000000000'
				?? str(w5caal,1)
				?? space(1)
				?? '00000000000000'
				?? '00000000'
				?? '00000000'
				?? space(75)
				?? iif(d5tifo='M',dtos(d5fpre),'00000000')
				?? iif(d5tifo='M',strcero(str((w5imre*100),15)),'000000000000000')
				?

				skip

				stor 0 to w5tota,w5neto,w5caal,w5otros,w5exen,w5impu,w5imre

			endif

		else

			skip -1

			?? '1'
			?? dtos(max(ctod('01/'+d5peri),d5fech))
			?? d5coco
			?? space(1)
			?? d5sucu
			?? '000000000000'+left(d5nume,8)
			?? '000000000000'+left(d5nume,8)
			?? w5tido
			?? w5nuic
			?? w5nocl
			?? strcero(str((w5tota*100),15))
			?? strcero(str(((w5otros)*100),15))
			?? strcero(str((w5neto*100),15))
			?? w5alic
			?? strcero(str(((w5impu)*100),15))
			?? '000000000000000'
			?? strcero(str((w5exen*100),15))
			?? '000000000000000'
			?? '000000000000000'
			?? '000000000000000'
			?? '000000000000000'
			?? '00'
			?? space(3)
			?? '0000000000'
			?? str(w5caal,1)
			?? space(1)
			?? '00000000000000'
			?? '00000000'
			?? '00000000'
			?? space(75)
			?? iif(d5tifo='M',dtos(d5fpre),'00000000')
			?? iif(d5tifo='M',strcero(str((w5imre*100),15)),'000000000000000')
			?

			skip

			stor 0 to w5tota,w5neto,w5caal,w5otros,w5exen,w5impu,w5imre


		endif

	enddo

endif

set alternate to
set console on
set alternate off
return('')
