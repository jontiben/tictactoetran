program tictactoe
	implicit none
	character(len=1) :: player = " "
	character, dimension(3,3) :: board
	character(len=4) :: raw_move
	integer :: i, i_move, line, turn = 1, temp_move, temp_line
	logical :: skip, win = .false.
	real :: randnum
	print*, "Enter a number to play tic-tac-toe!"
	print*, " 1 | 2 | 3"
	print*, "---+---+---"
	print*, " 4 | 5 | 6"
	print*, "---+---+---"
	print*, " 7 | 8 | 9"
	do
		!checks for winner
		if (win .eqv. .false.) then
			if (((board(1,1) == "x") .or. (board(1,1) == "o"))&
			.and. (((board(1,1) == board(1,2)) .and. (board(1,2) == board(1,3)))&
			.or. ((board(1,1) == board(2,2)) .and. (board(2,2) == board(3,3)))&
			.or. ((board(1,1) == board(2,1)) .and. (board(2,1) == board(3,1))))) then
				print*, board(1,1)," won!"
				win = .true.
			else if (((board(2,1) == "x") .or. (board(2,1) == "o"))&
			.and. ((board(2,1) == board(2,2)) .and. (board(2,2) == board(2,3)))) then
				print*, board(2,1)," won!"
				win = .true.
			else if (((board(3,1) == "x") .or. (board(3,1) == "o"))&
			.and. (((board(3,1) == board(3,2)) .and. (board(3,2) == board(3,3)))&
			.or. ((board(3,1) == board(2,2)) .and. (board(2,2) == board(1,3))))) then
				print*, board(3,1)," won!"
				win = .true.
			else if (((board(1,2) == "x") .or. (board(1,2) == "o"))&
			.and. ((board(1,2) == board(2,2)) .and. (board(2,2) == board(3,2)))) then
				print*, board(1,2)," won!"
				win = .true.
			else if (((board(1,3) == "x") .or. (board(1,3) == "o"))&
			.and. ((board(1,3) == board(2,3)) .and. (board(2,3) == board(3,3)))) then
				print*, board(1,3)," won!"
				win = .true.
			end if
		end if

		if (win .eqv. .false.) then
			!player turn
			if (MOD(turn,2) .ne. 0) then
				player = "x"
				read*, raw_move
				read(raw_move,*) i_move
				if (i_move < 1 .or. i_move >= 10) then
					print*, "invalid move, try again"
					cycle
				end if

				!check if space is occupied
				line = 1
				if (i_move == 4 ) then
					i_move = 1
					line = 2
				else if (i_move == 5) then
					i_move = 2
					line = 2
				else if (i_move == 6) then
					i_move = 3
					line = 2
				else if (i_move == 7) then
					i_move = 1
					line = 3
				else if (i_move == 8) then
					i_move = 2
					line = 3
				else if (i_move == 9) then
					i_move = 3
					line = 3
				end if
				if((board(i_move,line) .ne. "x") .and. (board(i_move,line) .ne. "o")) then
					board(i_move,line) = player
				else
					print*, "space is occupied"
					cycle
				end if
				turn = turn + 1

			!AI move
			else
				turn = turn + 1
				player = "o"
				skip = .false.
				do i = 1, 9
					i_move = i
					line = 1
					if (i_move == 4 ) then
						i_move = 1
						line = 2
					else if (i_move == 5) then
						i_move = 2
						line = 2
					else if (i_move == 6) then
						i_move = 3
						line = 2
					else if (i_move == 7) then
						i_move = 1
						line = 3
					else if (i_move == 8) then
						i_move = 2
						line = 3
					else if (i_move == 9) then
						i_move = 3
						line = 3
					end if
					temp_move = 0
					temp_line = 0
					!(debugging)
					!print*, "move> ", i_move, "line> ", line, "SKIP> ", skip
					if ((board(i_move,line) .ne. "x") .and. (board(i_move,line) .ne. "o")) then
						temp_move = i_move
						temp_line = line
						if (skip .eqv. .false.) then
							call random_number(randnum)
							if(randnum > 0.5) then
								board(i_move,line) = player
								skip = .true.
							end if
						end if
					end if
				end do
				if (skip .eqv. .false.) then
					print*, board(temp_move,temp_line)
					board(temp_move,temp_line) = player
					skip = .true.
				end if
				if (skip .eqv. .false.) then
					print*, "draw!"
					win = .true.
				end if
				print*, " ", board(1,1)," | ",board(2,1)," | ",board(3,1)
				print*, "---+---+---"
				print*, " ", board(1,2)," | ",board(2,2)," | ",board(3,2)
				print*, "---+---+---"
				print*, " ", board(1,3)," | ",board(2,3)," | ",board(3,3)
			end if
		cycle
		end if
	end do

end program tictactoe
