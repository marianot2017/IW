&&Saca printer y el tamaño de hoja del report para que adopte lo del printer seleccionado de la PC
clear
close data
w_dire='report\'
cant=adir(w_file,w_dire+'*.frx')
wait wind 'Se encontraron '+str(cant,5)+' reports' time 1
		set alter to memorep2.txt
		set alter on
		set conso off
		j=0
for i=1 to cant
	w_nomb=w_dire+w_file(i,1)
	use &w_nomb.
	locate for hpos>80000 and platform='WINDOWS'
	if !found()

		go top
		
		if fontface='Letter Gothic'
			j=j+1
		
			wait wind allt(str(j,4))+") Saca printer y tamaño de hoja de : "+w_nomb nowait

			?allt(str(j,4))+") "+w_nomb
		
			replace tag with '',tag2 with ''

		endif
	
		use
	else
		wait wind "Printer Horizontal : "+w_nomb nowait
		use
	endif
endfor
		set conso on
		set alter to
		set alter off

wait clear
clear
return

