import React from 'react';
import Select from 'react-select';

function ControlPanel(props) {

    const source_in_options = [
      {value: 'Haskell', label: 'Haskell'},
      {value: 'C', label: 'C'}
    ];
    const optimizations = [
      {value: 'O0', label: 'O0'},
      {value: 'O1', label: 'O1'},
      {value: 'O2', label: 'O2'}
    ];
    const source_out_options = [
      {value: 'X86', label: 'X86'},
      {value: 'LLVM', label: 'LLVM'}
    ];
    const sample_prog_opts = [
      {value: 'hello_world', label: 'hello_world'},
      {value: 'fib', label: 'fib'},
      {value: 'loop', label: 'loop'},
      {value: 'enum', label: 'enum'}
    ];

    const {source_in, source_out, optimization, program_sample} = props;
    console.log("render ControlPanel with ", props);
    console.log(source_out);
    return (
      <div id={'editor-control-panel'}>
        <Select
          value={{value: source_in, label: source_in}}
          options={source_in_options}
          className={'editor-control-container'}
          placeholder={'Source in'}
          onChange={props.source_in_onChange}
        />
        <Select
          value={{value: optimization, label: optimization}}
          options={optimizations}
          className={'editor-control-container'}
          placeholder={'Optimization'}
          onChange={props.opt_onChange}
        />
        <Select
          value={{value: source_out, label: source_out}}
          options={source_out_options}
          className={'editor-control-container'}
          placeholder={'Source out'}
          onChange={props.source_out_onChange}
        />
        <Select
          value={{value: program_sample, label: program_sample}}
          options={sample_prog_opts}
          className={'editor-control-container'}
          placeholder={'Program'}
          onChange={props.program_sample_onChange}
        />
      </div>
    );
}

export default ControlPanel;
