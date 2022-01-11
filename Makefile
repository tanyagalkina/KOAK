##
## EPITECH PROJECT, 2021
## bs_wolframe
## File description:
## Makefile
##

BINARY_PATH 	:=	$(shell stack path --allow-different-user --local-install-root)

NAME 	= 	koak

all: $(NAME)

$(NAME):
	stack build --allow-different-user
	cp $(BINARY_PATH)/bin/$(NAME)-exe ./$(NAME)

clean:
	stack clean --allow-different-user

fclean: clean
	rm -rf $(NAME)

re: fclean all