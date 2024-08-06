{ lib, python3, ... }:

python3.pkgs.buildPythonApplication rec {
  pname = "protobuf";
  version = "3.20.3";
  pyproject = true;

  src = python3.pkgs.fetchPypi {
    inherit pname version;
    hash = "sha256-LjQnQpyc/+vyWUkb4K9wGJYH82XC9Bx8N2SvbzNxBfI=";
  };

  nativeBuildInputs = [
    python3.pkgs.setuptools
    python3.pkgs.wheel
  ];

  meta = with lib; {
    description = "";
    homepage = "https://pypi.org/project/protobuf/";
    license = licenses.bsd3;
    maintainers = with maintainers; [ ];
    mainProgram = "protobuf";
  };
}
