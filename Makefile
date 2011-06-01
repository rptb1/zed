# Makefile for zed

CC = gcc
CFLAGS = -g -std=iso9899:1990 -pedantic-errors -Wall
RM = rm -f

zed: zed.o
