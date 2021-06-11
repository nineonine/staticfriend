import React from 'react';
import Select from 'react-select';

function ControlPanel() {

const options = [
  {value: 'Haskell', label: 'Haskell'},
  {value: 'C', label: 'C'}
];
const optimizations = [
  {value: 'O0', label: 'O0'},
  {value: 'O1', label: 'O1'},
  {value: 'O2', label: 'O2'}
];
  return (
    <div id={'editor-control-panel'}>
      <Select
        value={'Haskell'}
        options={options}
      />
      <Select
        value={'O0'}
        options={optimizations}
      />
    </div>
  );
}

export default ControlPanel;
