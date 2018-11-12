*)-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
*)-H ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ Sys.Info ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ
*)-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
*)-H Programa        : IWP015L.PRG -> Listador de mov.complementarios
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

*) Variables-------------------------------------------------------------------

w_pfile='15l'
w_prog =upper(prompt())
w_title=db_name('f15')
w_alta =.f.

w_pede=space(2)+space(4)
w_peha='99/9999'

w_drep=''
w_hrep=''

*) Apertura de archivos--------------------------------------------------------

fieldvec[1]='F00'
fieldvec[2]='F04'
fieldvec[3]='F05'
fieldvec[4]='F13'
fieldvec[5]='F15'
fieldvec[6]='F21'

use (d0f00) in 0 alias 'f00'						&&(Tablas)
use (d0f04) in 0 alias 'f04' order 'd4peri'	&&(Periodos)
use (d0f05) in 0 alias 'f05' order 'd5peri'	&&(Comprobantes)
use (d0f13) in 0 alias 'f13' order 'd13peri' &&(Anticipos y retenciones)
use (d0f15) in 0 alias 'f15' order 'd15peri' &&(Mov.Complementarios)
use (d0f21) in 0 alias 'f21' order 'd21tip'	&&(Tablas)

if neterr()
	pop key
	pop menu _msysmenu
	fieldvec=''
	=closall()
	return
endif

fieldvec=''

*)-----------------------------------------------------------------------------

sele f15
go top
w_pede=d15peri
go bott
w_peha=d15peri

do ssm998.mpr
do iws015l.spr

pop key
pop menu _msysmenu
=closall()

dele file &d0f99.c.dbf

return

***vfp***
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
proc cons15l
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
	on key label f9 do cons15l0
	keyboard '{f9}'
return

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
proc cons15l0
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
on key label f9
***vfp***

if .not. valid15l(.t.)
	wait wind 'Verifique los datos ingresados...'
	return
endif

=gentmp()

w_drep=w_pede
w_hrep=w_peha

if used('f99c')
	sele f99c
	zap
else
	sele 0
	create dbf &d0f99.c (d15peri c(7),d15tipo c(1),d21des c(40),d15impo n(19,2))
	use &d0f99.c alias f99c exclusive
endif

sele f15
set rela to 'TC'+d15tipo into f21

=seek(ptoi(w_pede),'f15')
do while !eof('f15') and ptoi(f15.d15peri)<=ptoi(w_peha)
	insert into f99c;
		values (f15.d15peri,f15.d15tipo,f21.d21des,f15.d15impo)
	skip in f15
enddo

if !p_cancela
	return
endif

do repdest &&(Setea variables internas del reporte)

sele f99c
go top
if !empty(p_repclaus) &&(debe ser preview o to printer)
	p_report='iwr015l'
	=rep_form()
	p_report=''
endif

do represet &&(Resetea estado)

sele f15
set relation to

return

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
FUNC valid15l
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PARAMETERS w_show

if empty(parameters())
	w_show=.f.
endif

PRIVATE w_retu,w_vari
w_retu=.t.
w_vari=varread()

if w_vari='W_PEDE' or w_show
	w_pede=strcero(left(w_pede,2))+'/'+strcero(right(w_pede,4))
	show get w_pede
	if lastkey()=p_f4
		do fhelp with 'W_PEDE','','iwp015','f15','d15peri','d15peri','',(p_char),'w_retu'
	   wkperi=PtoI(w_pede)
   	if !seek(wkperi,'f15')
	   	if !w_show
		   	wait window 'Perํodo Inexistente'
	   	endif
		   w_retu=.f.
		endif
   endif
endif

if w_vari='W_PEHA' or w_show
	w_peha=strcero(left(w_peha,2))+'/'+strcero(right(w_peha,4))
	show get w_peha
	if lastkey()=p_f4
		do fhelp with 'W_PEHA','','iwp015','f15','d15peri','d15peri','',(p_char),'w_retu'
	   wkperi=PtoI(w_peha)
   	if !seek(wkperi,'f15')
   		if !w_show
		   	wait window 'Perํodo Inexistente'
		   endif
	   	w_retu=.f.
		endif
   endif
endif

if ptoi(w_pede)>ptoi(w_peha)
	if !w_show
		wait wind 'Hasta debe ser mayor o igual que desde...'
	endif
	w_retu=.f.
endif
return(w_retu)

***puesto adrede para que incluya el report al proyecto**
report form iwr015l
