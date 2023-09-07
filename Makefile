######################################################
#
#  Makefile for test-hdl with verilog-ts-mode
#
#  Copyright (c) 2022-2023 Gonzalo Larumbe
#  All rights reserved.
# 
######################################################

# Variables
TEST_HDL_PATH = test-hdl
ERT_TESTS = $(TEST_HDL_PATH)/ert-tests.sh
LANGUAGE = verilog
PACKAGE = verilog-ts-mode

include $(TEST_HDL_PATH)/Makefile.mk
