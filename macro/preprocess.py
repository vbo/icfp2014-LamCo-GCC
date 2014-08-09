import sys
import os.path
import re

# !def macro something - define macro (local to compilation unit)
# #something macro
# $something: define label, $something - use label
# @something call external func


# used for basic spellchecking
gcc_instructions = set([
    "LDC", "LD", "ADD", "SUB", "MUL", "DIV",
    "CEQ", "CGT", "CGTE", "ATOM", "CONS", "CAR",
    "CDR", "SEL", "JOIN", "LDF", "AP", "RTN", "DUM",
    "RAP", "STOP", "TSEL", "TAP", "TRAP", "ST",
    "DBUG", "BRK"
])


class Error(Exception):
    pass


class CompilationUnit(object):

    ALLOW_NO_RTN = False
    PRINT_UNIT_NAME = True
    PRINT_INSTR_CNT_DBUG = False
    EOI_RE = r"(?:$|;.*)"

    def __init__(self, name, source):
        # pre-cleanup, macro-replace, local jumps
        self.name = name
        self.labels = {}
        self.lines = []
        self.dep_funcs = set()
        self.instructions_count = 0
        lines = source.split("\n")
        for line in lines:
            line = line.strip()
            if not line.startswith('!'):
                continue
            def_match = re.match(r'^!def\s+(\w+)(.*)$', line)
            if def_match:
                k = def_match.groups()[0]
                v = def_match.groups()[1]
                source = re.sub("#" + k + r"\b", "\n".join(v.split(',')), source)
        expect_line = False
        has_rtn = False
        line_no = 0
        for line in source.split("\n"):
            line_no += 1
            line = line.strip()
            if not line or line.startswith(';') or line.startswith('!'):
                continue
            label_match = re.match(r'\$(\w+):' + self.EOI_RE, line)
            if label_match:
                label_name = label_match.groups()[0]
                if label_name in self.labels:
                    raise Error("Duplicate label $%s (line_no: %s, unit: %s)" % (
                        label_name, line_no, self.name))
                self.labels[label_name] = self.instructions_count
                expect_line = True
                continue
            if self.PRINT_INSTR_CNT_DBUG:
                self.lines.append("LDC -42")
                self.lines.append("DBUG")
                self.instructions_count += 2
            dep_call_match = re.search(r'@(\w+)', line)
            if dep_call_match:
                dep_name = dep_call_match.groups()[0]
                self.dep_funcs.add(dep_name)
                # do not exec continue - it's just a normal line
            instr_match = re.match(r'(\w+).*', line)
            if not instr_match:
                raise Error("Bad line: |%s| (line_no: %s, unit: %s)" % (line, line_no, self.name))
            instr = instr_match.groups()[0]
            if instr not in gcc_instructions:
                raise Error("No such instruction: |%s| (line: %s, line_no: %s, unit: %s)" % (
                    instr, line, line_no, self.name))
            if instr == "RTN":
                has_rtn = True
            expect_line = False
            self.lines.append(line)
            self.instructions_count += 1
        if expect_line:
            raise Error("Expected line after '%s'" % (self.lines[-1]))
        if not self.ALLOW_NO_RTN and not has_rtn:
            raise Error("RTN required at unit: %s" % (self.name,))
        if self.PRINT_UNIT_NAME:
            self.lines[0] += '; unit ' + self.name

    def generate_code(self, code_ref, dep_refs):
        code = "\n".join(self.lines)
        for lab, ref in self.labels.iteritems():
            code = re.sub(r"\$" + lab + r"\b", str(code_ref + ref), code)
        for dep in self.dep_funcs:
            code = re.sub(r"@" + dep + r"\b", str(dep_refs[dep]), code)
        return code


class Linker(object):

    def __init__(self, source_loader):
        self.source_loader = source_loader
        self.units = {}
        self.units_in_order = []
        self.code_pos = 0

    def link(self, unit):
        self._link_all(unit)
        code = []
        for u in self.units_in_order:
            dep_refs = {}
            for dep in u.dep_funcs:
                dep_refs[dep] = self.units[dep].start
            code.append(u.generate_code(u.start, dep_refs))
        return "\n".join(code)

    def _link_all(self, unit):
        if unit.name in self.units:
            raise Error("%s unit compiled more then once" % (unit.name,))
        self.units[unit.name] = unit
        self.units_in_order.append(unit)
        unit.start = self.code_pos
        self.code_pos += unit.instructions_count
        for d in unit.dep_funcs:
            if d in self.units:
                continue
            self._link_all(self.source_loader(d))


if __name__ == '__main__':
    def unit_from_file(name):
        return CompilationUnit(name, file(unit_from_file.source_root + "/" + name + ".gcc").read())

    if len(sys.argv) < 3:
        print "Usage: %s <path_to_prog> <include_dir>" % sys.argv[0]
        exit()

    p = os.path
    prog_file = p.realpath(sys.argv[1])
    prog_name = p.splitext(p.basename(prog_file))[0]
    include_dir = p.realpath(sys.argv[2])
    unit_from_file.source_root = include_dir
    comp = Linker(unit_from_file)
    code = comp.link(CompilationUnit(prog_name, file(prog_file).read()))
    print code


