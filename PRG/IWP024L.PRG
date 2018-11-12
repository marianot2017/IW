*)-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
*)-H ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ Sys.Info ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ
*)-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
*)-H Programa        : IWP024L.PRG -> Listador de Perc./Ret./Otros
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

w_prog=upper(prompt())
w_alta=.f.

*) Variables del report--------------------------------------------------------

w_pfile='24l'
w_title=db_name('f24')

w_dcod=space(3)
w_hcod='999'

w_drep=''
w_hrep=''

*) Apertura de archivos--------------------------------------------------------

fieldvec[1]='F24'

use (d0f24) in 0 alias 'f24' order 'd24codi' &&(Perc./Ret./Otros)

if neterr()
	pop key
	pop menu _msysmenu
	fieldvec=''
	=closall()
	return
endif

=creatipos('V')	&& perc/ret/otros

fieldvec=''

*)-----------------------------------------------------------------------------

do ssm998.mpr
do iws024l.spr

pop key
pop menu _msysmenu
=closall()

dele file &d0f99.c.dbf
=borratipos()

return

***vfp***
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
proc cons24l
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
	on key label f9 do cons24l0
	keyboard '{f9}'
return

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
proc cons24l0
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
on key label f9
***vfp***

if .not. valid24l(.t.)
	wait wind 'Verifique los datos ingresados...'
	return
endif

=gentmp()

w_drep=w_dcod
w_hrep=w_hcod

if used('f99c')
	sele f99c
	use
	dele file &d0f99.c.dbf
endif

sele 0
if !used('f01')
	use (d0f01) in 0 alias 'f01' order 'd1tip'  &&(Tablas)
endif	

select *;
	from f24;
	where between(d24codi,w_dcod,w_hcod);
	into dbf &d0f99.c order by d24codi
use &d0f99.c alias f99c exclusive
	
if !p_cancela
	return
endif

do repdest &&(Setea variables internas del reporte)

sele f99c
go top
if !empty(p_repclaus) &&(debe ser preview o to printer)
	p_report='iwr024l'
	=rep_form()
	p_report=''
endif

do represet &&(Resetea estado)

return

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
FUNC valid24l
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PARAMETERS w_show

if empty(parameters())
	w_show=.f.
endif

PRIVATE w_retu,w_vari
w_retu=.t.
w_vari=varread()

if w_vari='W_DCOD' or w_show
	w_dcod=strcero(w_dcod)
	show get w_dcod
	if lastk()=p_f4
		do fhelp with 'W_DCOD','','iwp024','f24','d24codi','d24codi','',(p_strc),'w_retu'
		use in f01
	endif
endif

if w_vari='W_HCOD' or w_show
	w_hcod=strcero(w_hcod)
	show get w_hcod
	if lastk()=p_f4
		do fhelp with 'W_HCOD','','iwp024','f24','d24codi','d24codi','',(p_strc),'w_retu'
		use in f01
	endif
	if w_dcod>w_hcod
		if !w_show
			wait wind 'Hasta debe ser mayor o igual que desde...'
		endif
		w_retu=.f.
	endif
endif
return(w_retu)

***puesto adrede para que incluya el report al proyecto**
report form iwr024l
