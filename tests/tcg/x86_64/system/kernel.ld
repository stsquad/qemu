SECTIONS {
	. = 0x100000;

	.text : {
		__load_st = .;
		*(.head)
		*(.text)
	}

	.rodata : {
		*(.rodata)
	}

        /* Keep build ID and PVH notes in same section */
        .notes :  {
               *(.note.*)
        }

        .data : {
		*(.data)
		__load_en = .;
	}

	.bss : {
		*(.bss)
		__bss_en = .;
	}

}
