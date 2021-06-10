import React from 'react';
import Dropdown from 'react-dropdown';
import 'react-dropdown/style.css';

function ControlPanel() {

const options = ['Haskell', 'C'];
const optimizations = ['O0', '01', 'O2']
  return (
    <div id={'editor-control-panel'}>
      <Dropdown
        options={options}
	value={options[0]}
	placeholder="Select Language"
      />
      <Dropdown
        options={optimizations}
	value={optimizations[0]}
	placeholder="Optimization Level"
      />
    </div>
  );
}

export default ControlPanel;
