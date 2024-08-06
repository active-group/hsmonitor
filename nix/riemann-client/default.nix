{ lib, python3, ... }:

python3.pkgs.buildPythonApplication rec {
  pname = "riemann-client";
  version = "7.0.0";
  pyproject = true;

  src = python3.pkgs.fetchPypi {
    inherit pname version;
    hash = "sha256-kj+nysES8JfFtjCsbhZsl0vRqSE3PLZ6+nXbco+xD7U=";
  };

  nativeBuildInputs = [
    python3.pkgs.setuptools
    python3.pkgs.wheel
  ];

  propagatedBuildInputs = with python3.pkgs; [
    click
    (import ../protobuf { inherit lib python3; })
  ];

  passthru.optional-dependencies = with python3.pkgs; {
    docs = [
      sphinx
      sphinx-rtd-theme
    ];
  };

  pythonImportsCheck = [ "riemann_client" ];

  meta = with lib; {
    description = "A Riemann client and command line tool";
    homepage = "https://pypi.org/project/riemann-client/";
    license = licenses.mit;
    maintainers = with maintainers; [ ];
    mainProgram = "riemann-client";
  };
}
