*)-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
*)-H ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ Sys.Info ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ
*)-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
*)-H Programa        : IWP013ML.PRG -> Listador de Min.y saldo a favor
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

w_pfile='13ml'
w_alias='f13'
w_title='MINIMOS Y SALDO A FAVOR'

w_pede=space(2)+'/'+space(4)
w_peha='99/9999'

w_drep=''
w_hrep=''

*) Apertura de archivos--------------------------------------------------------

fieldvec[1]='F00'
fieldvec[2]='F01'
fieldvec[3]='F04'
fieldvec[4]='F13'

if !w_call

	use (d0f00) in 0 alias 'f00'						&&(Parametros)
	use (d0f01) in 0 alias 'f01' order 'd1tip'	&&(Tablas)
	use (d0f04) in 0 alias 'f04' order 'd4peri'	&&(Periodos)
	use (d0f13) in 0 alias 'f13' order 'd13peri' &&(Anticipos y retenciones)
	
	if neterr()
		pop key
		pop menu _msysmenu
		fieldvec=''
      =closall()
      return
	endif
	
endif

fieldvec=''

*)-----------------------------------------------------------------------------

sele f13
go top
w_pede=d13peri
go bott
w_peha=d13peri

do ssm998.mpr
do iws013ml.spr

pop key
pop menu _msysmenu
=closall()

dele file &d0f99.c.dbf

return

***vfp***
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
proc cons13ml
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
	on key label f9 do cons13ml0
	keyboard '{f9}'
return

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
proc cons13ml0
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
on key label f9
***vfp***

if .not. valid13ml(.t.)
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
	create dbf &d0f99.c;
		(d13peri c(7),d13juri c(1),d13mini n(19,2),d13rete n(19,2),d13otpa n(19,2),d13safa n(19,2),d1des c(40))
	use &d0f99.c alias f99c exclusive
endif

sele f13
set rela to 'JU'+d13juri into f01

=seek(ptoi(w_pede),w_alias)
scan while !eof(w_alias) and ptoi(f13.d13peri)<=ptoi(w_peha)
	insert into f99c values;
		(f13.d13peri,f13.d13juri,f13.d13mini,f13.d13rete,f13.d13otpa,f13.d13safa,f01.d1des)
endscan

sele f13
set rela to

if !p_cancela
	return
endif

do repdest &&(Setea variables internas del reporte)

sele f99c
go top
if !empty(p_repclaus) &&(debe ser preview o to printer)
	p_report='iwr013ml'
	=rep_form()
	p_report=''
endif

do represet &&(Resetea estado)

return

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
FUNC valid13ml
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
		do fhelp with 'W_PEDE','','iwp013','f13','d13peri','d13peri','',(p_char),'w_retu'
   endif   
   if !seek(PtoI(w_pede),'f13')
   	if !w_show
	   	wait window 'Perํodo Inexistente'
	   endif
	   w_retu=.f.
	endif
endif

if w_vari='W_PEHA' or w_show
	w_peha=strcero(left(w_peha,2))+'/'+strcero(right(w_peha,4))
	show get w_peha
	if lastkey()=p_f4
		do fhelp with 'W_PEHA','','iwp013','f13','d13peri','d13peri','',(p_char),'w_retu'
   endif
   if !seek(ptoi(w_peha),'f13')
   	if !w_show
	   	wait window 'Perํodo Inexistente'
	   endif
	   w_retu=.f.
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
report form iwr013ml
