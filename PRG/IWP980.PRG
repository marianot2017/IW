*)-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
*)-H ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ Sys.Info ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ
*)-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
*)-H Programa        : IWP980.PRG -> Edicion de formularios
*)-H
*)-H Release		 : 01
*)-H Fecha de inicio : 22/12/1998.-
*)-H Fecha de fin    : 29/12/1998.-
*)-H
*)-H Actualizaciones
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
w_mwidth =set('memowidth')
set memowidth to 161

*) Variables internas----------------------------------------------------------

w_prog=upper(prompt())
w_alta=.f.
w_agre=.not. mprot(p_user,w_prog,'Registro','Agregar')
w_modi=.not. mprot(p_user,w_prog,'Registro','Modificar')
w_recn=0
w_modicampo=.f.

w_pfile='980'
w_alias='ffo'
w_clave='m.d55codi'
w_title='EDICION DE FORMULARIOS'
w_grupo=.f.

*) Apertura de archivos--------------------------------------------------------

fieldvec[1]='FEX'
fieldvec[2]='FFO'

use (d0fex) in 0 alias 'fex' order 'd44clav' excl
use (d0ffo) in 0 alias 'ffo' order 'd55codi' excl

if neterr()
	pop key
	pop menu _msysmenu
	set memowidth to (w_mwidth)
	fieldvec=''
	=closall()
	return
endif

fieldvec=''

for wFile=1 to 40
   strnum=strcero(str(wFile,2))
   campo='d0f'+StrNum
   if !type(campo)='U'
  		n0Arch=&Campo  &&(Reemplazo la palabar Archivo por n0arch)
      if mfile(n0Arch+'.dbf')
      	wait wind nowait 'Abriendo Archivo: '+n0Arch+'...'
      	do while .t.
      		fieldvec[1]='F'+strnum
      		use (n0arch) in 0 alias ('f'+strnum)
      		if !neterr()      			
      			exit
      		endif
      		fieldvec[1]=''
      	enddo
      endif
   endif
endfor
wait clear

*)-----------------------------------------------------------------------------

define window iws980e at 0,0 size srows(),scols();
	font 'Fixedsys',9 title (w_title) zoom float grow system
zoom windo iws980e max

set skip of menu _msysmenu .t.
do iwm980.mpr
=skipbar('registro','all',.t.)
=skipbar('registro','salir',.f.)
on key label f1 do nada

do iws980.spr

rele wind iws980e
pop key
do ssm995.mpr	&& R.01 Mariano (puesto porque en vfp quedan activas los key del menu usado y de esta manera se limpian)
pop menu _msysmenu
set memowidth to (w_mwidth)
=closall()

dele file &d0f99.c.dbf

return

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
FUNC valid980
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PARAMETERS w_show

if empty(parameters())
	w_show=.f.
endif

w_retu=.t.
w_vari=varread()
w_vread='m.'+w_vari

if w_vari='D55CODI' or w_show
	if empty(m.d55codi)
		if !w_show
			wait wind 'Debe especificar un formulario...'
		endif
		w_retu=.f.
	endif
endif
return(w_retu)

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PROC clav980
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
w_orde=order(w_alias)
set orde to 'd55codi' in (w_alias)

if !seek(w_pref+&w_clave,w_alias)
	=work980(.t.)
else
	=work980()
endif

set orde to (w_orde) in (w_alias)
return

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PROC work980
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PARAMETERS w_eof

set mess to

=skipbar('registro','Agregar'  ,.f.)
=skipbar('registro','Modificar',.f.)
=skipbar('registro','Ayuda expresiones',.f.)
=skipbar('registro','Imprimir',.f.)

sele (w_alias)
if w_eof
	if sino('Desea agregar un nuevo formulario ?.','N')='S'
		append blank
		repl d55codi with m.d55codi,d55memo with '0-------010-------020-------030-------040-------050-------060-------070-------080-------090-------100-------110-------120-------130-------140-------150-------160'+chr(13)+' '
	else
		return
	endif
endif
set message to 'Ingrese o modifique las filas del formulario'
modi memo d55memo window iws980e nomenu
close memo d55memo

=skipbar('registro','all',.t.)
=skipbar('registro','salir',.f.)

set mess to csalida
return

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PROC actu980
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
wait wind 'Actualizando formulario...' time .4
keyb '{ctrl+w}'
return

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PROC brow980
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
sele fex
go top
brow fields;
	d44clav :r:h='Ex':p='@K',;
	d44nomb :r:h='Nombre':p='@K';
	nome noap noed node norm in screen title 'EXPRESIONES' font 'arial',10
sele (w_alias)
return

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PROC impre980
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
push menu _msysmenu
do ssm998.mpr
do getdest.spr
pop menu _msysmenu
return

***vfp***
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
proc cons980
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
	on key label f9 do cons980x
	keyboard '{f9}'
return

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
proc cons980x
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
on key label f9
***vfp***

if used('f99c')
	sele f99c
	zap
else
	sele 0
	create dbf &d0f99.c (d99expr c(161),d99font c(1),d99grup l(1))
	use &d0f99.c alias f99c exclusive
endif

wPrueba=.t.
priv wcol,wrow,wScreen
wcol=col()
wrow=row()

t_cara=80
w0doon=''
w0doof=''

set century off

=f_set(m.d55Codi)

w0Len  = ''
w0Ini  = ''
w0doon = ''
w0doof = ''

=f_cabe()
iLine=1
do while iLine <= f_cal_det .and. p_cancela
   =f_deta(iLine)
   iLine=iLine+1
enddo
=f_pie()
=f_rel()

do repdest &&(Setea variables internas del reporte)
sele f99c
go top	
if !empty(p_repclaus) &&(debe ser preview o to printer)
	p_report='iwr005e'
	=rep_form()
	p_report=''
endif	
do represet &&(Resetea estado)
sele ffo

set century on

return
