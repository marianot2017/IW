*)-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
*)-H ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ Sys.Info ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ
*)-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
*)-H Programa        : IWP865.PRG -> Listado trimestral de cpra-vtas
*)-H
*)-H Fecha de inicio :
*)-H Fecha de fin    :
*)-H
*)-H Updates
*)-H
*)-H Fecha       Programmer      Comentarios
*)-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
*)-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PARAMETERS w_pref,w_call

PRIVATE ALL LIKE w_*
if empty(parameters())
	w_pref=''
	w_call=.f.
endif

*) Salvado de estados anteriores-----------------------------------------------

push key clear		  &&(Salva estados de teclas)
push menu _msysmenu &&(Salva estado de menu)
set skip of menu _msysmenu .t.

*) Variables internas----------------------------------------------------------

w_prog =upper(prompt())
w_alta =.f.
w_pfile='865'
w_title='LISTADO TRIMESTRAL COMPRAS-VENTAS'

*) Apertura de archivos--------------------------------------------------------

fieldvec[1]='F00'
fieldvec[2]='F01'
fieldvec[3]='F02'
fieldvec[4]='F03'
fieldvec[5]='F04'
fieldvec[6]='F05'
fieldvec[7]='F06'
fieldvec[8]='F12'

use (d0f00) in 0 alias 'f00'					  &&(Parametros Empresa)
use (d0f01) in 0 alias 'f01' order 'd1tip'  &&(Tablas)
use (d0f02) in 0 alias 'f02' order 'd2clie' &&(Clientes)
use (d0f03) in 0 alias 'f03' order 'd3prov' &&(Proveedores)
use (d0f04) in 0 alias 'f04' order 'd4peri' &&(Periodos)
use (d0f05) in 0 alias 'f05' order 'd5pefe' &&(Comprobantes)
use (d0f06) in 0 alias 'f06' order 'd6inte' &&(Arch.Alternativo)
use (d0f12) in 0 alias 'f12'					  &&(Parametros sistema)

if neterr()
	pop key
	pop menu _msysmenu
	fieldvec=''
	=closall()
	return
endif

fieldvec=''

*) Variables-------------------------------------------------------------------

go top in f04

wtrim ='1'
wano  =right(f04.d4peri,4)
wdperi=''
whperi=''
wsolo ='S'
wdeta ='N'

w_vta=.f.
w_cpra=.f.



w_ocv1=f00->p0ocv1
w_ocv2=f00->p0ocv2
w_ocv3=f00->p0ocv3

w_occ1=f00->p0occ1
w_occ2=f00->p0occ2
w_occ3=f00->p0occ3


*)-----------------------------------------------------------------------------

=SetComp() &&(Setea filtros de comprobante DB CR y su signo)

do ssm998.mpr
do iws865.spr

pop key
pop menu _msysmenu
=closall()
return

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
FUNC valid865
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PARAMETERS w_show

if empty(parameters())
	w_show=.f.
endif

PRIVATE w_retu,w_vari
w_retu=.t.
w_vari=varread()

if w_vari='WTRIM' or w_show
	if !wtrim$'01234'
		if !w_show
			wait wind 'Opciones vแlidas: 1,2,3,4'
		endif
		w_retu=.f.
	else
		if empty(wdperi) and empty(whperi)
			do case
			case wTrim='1'
				wdperi='01'
				whperi='03'
			case wTrim='2'
				wdperi='04'
				whperi='06'
			case wTrim='3'
				wdperi='07'
				whperi='09'
			case wTrim='4'
				wdperi='10'
				whperi='12'
			otherwise
				wdperi='01'
				whperi='12'
			endcase
			wdperi=wdperi+'/'+wAno
			whperi=whperi+'/'+wAno
			show get wdperi
			show get whperi			
		endif
	endif
endif

if w_vari='WANO' or w_show
	if wano<'1985'
		if w_show
			wait wind 'A๑o no permitido...'
		endif
		w_retu=.f.
	endif
endif

if w_vari='WDPERI' or w_show
	wdperi=strcero(left(wdperi,2))+'/'+strcero(right(wdperi,4))
	show get wdperi
	if lastkey()=p_f4
		do fhelp with 'WDPERI','','iwp004','f04','d4peri','d4peri','',(p_char),'w_retu'
	endif
	if !seek(ptoi(wdperi),'f04')
		if !w_show
			wait window 'Perํodo Inexistente.'
		endif
		w_retu=.f.
	endif
endif

if w_vari='WHPERI' or w_show
	whperi=strcero(left(whperi,2))+'/'+strcero(right(whperi,4))
	show get whperi
	if lastkey()=p_f4
		do fhelp with 'WHPERI','','iwp004','f04','d4peri','d4peri','',(p_char),'w_retu'
		if !seek(ptoi(whperi),'f04')
			if !w_show
				wait window 'Perํodo Inexistente.'
			endif
			w_retu=.f.
		endif
	endif
	if ptoi(wdperi)>ptoi(whperi)
		if !w_show
			wait wind 'Hasta debe ser mayor o igual que desde...'
		endif
		w_retu=.f.
	endif
endif

if w_vari='WSOLO' or w_show
	if !wsolo$'SN'
		if !w_show
			wait wind 'Opciones vแlidas: S,N'
		endif
		w_retu=.f.
	endif
endif

if w_vari='WDETA' or w_show
	if !wdeta$'SN'
		if !w_show
			wait wind 'Opciones vแlidas: S,N'
		endif
		w_retu=.f.
	endif
endif

if w_vari='W_VTA'
	=mchkbox(@w_vta)
endif

if w_vari='W_CPRA'
	=mchkbox(@w_cpra)
endif

return(w_retu)

***vfp***
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
proc cons865
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
	on key label f9 do cons865x
	keyboard '{f9}'
return

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
proc cons865x
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
on key label f9
***vfp***

if !valid865(.t.)
	wait wind 'Verifique los datos ingresados...'
	return
endif

w_tipo=iif(w_vta,'V','')+iif(w_cpra,'C','')

if creamov(wDperi,wHperi,w_tipo,'1')
	if !eof('f9c')
		if wSolo='S'
			if Sele860('S')
				Sele f9c
				set filt to !empty(d5sele)
				go top
				=list865()
				sele f9c
				set filt to
			else
				=advsuave('No hay Movimientos que Cumplan condici๓n...','Informaci๓n')
			endif
		else
			=Sele860('S')
			=list865()
		endif
	else
		=advsuave('No hay Movimientos...','Informaci๓n')
	endif
endif

sele f9c
use
sele f05
dele file &d0f99.c.dbf
dele file &d0f99.c.cdx
return

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PROC list865
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
do repdest &&(Setea variables internas del reporte)

sele f9c
go top
if !empty(p_repclaus) &&(debe ser preview o to printer)
	if w_vta
		w_reptit='LISTADO VENTAS PARA CITI'
		t_leye='Desde: '+wdperi+', Hasta: '+whperi+;
			spac(11)+'Tipo de Comprobantes: FA=Fact.,CR=Cr้d.,DB=D้bito,TI=Tick.,RE=Rec.,FC=Fact.Cr้d.,RF=Rec.-Fact.,TF=Tick.-Fact.'
		p_report='iwr865'
		=rep_form()
		p_report=''
	endif
	if w_cpra
		w_reptit='LISTADO DE COMPRAS PARA CITI'
		t_leye='Desde: '+wdperi+', Hasta: '+whperi+;
			spac(11)+'Tipo de Comprobantes: FA=Fact.,CR=Cr้d.,DB=D้bito,TI=Tick.,RE=Rec.,RF=Rec.-Fact.,LV=Liq.Varias,TF=Tick.-Fact.'
		p_report='iwr865'
		=rep_form()
		p_report=''
	endif
endif

do represet &&(Resetea estado)
return

***puesto adrede para que incluya el report al proyecto**
report form iwr865
