import React from 'react';
import Select from 'react-select';

function ControlPanel(props) {

    const source_options = [
      {value: 'Haskell', label: 'Haskell'},
      {value: 'C', label: 'C'}
    ];
    const optimizations = [
      {value: 'O0', label: 'O0'},
      {value: 'O1', label: 'O1'},
      {value: 'O2', label: 'O2'}
    ];
    const target_options = [
      {value: 'X86', label: 'X86'},
      {value: 'LLVM', label: 'LLVM'}
    ];
    const sample_prog_opts = [
      {value: 'HelloWorld', label: 'HelloWorld'},
      {value: 'Fib', label: 'Fib'},
      {value: 'Loop', label: 'Loop'},
      {value: 'Enum', label: 'Enum'}
    ];

    const { source, target, optimization, program_sample
          , source_onChange, opt_onChange, target_onChange
          , program_sample_onChange
          , target_loc } = props;
    return (
      <div id={'editor-control-panel'}>
        <Select
          value={{value: source, label: source}}
          options={source_options}
          className={'editor-control-container'}
          placeholder={'Source in'}
          onChange={source_onChange}
        />
        <Select
          value={{value: optimization, label: optimization}}
          options={optimizations}
          className={'editor-control-container'}
          placeholder={'Optimization'}
          onChange={opt_onChange}
        />
        <Select
          value={{value: target, label: target}}
          options={target_options}
          className={'editor-control-container'}
          placeholder={'Source out'}
          onChange={target_onChange}
        />
        <Select
          value={{value: program_sample, label: program_sample}}
          options={sample_prog_opts}
          className={'editor-control-container'}
          placeholder={'Program'}
          onChange={program_sample_onChange}
        />
        <span style={{'margin-top':'25px'}}>Selected Line:  {target_loc}</span>
        <div id={"fix-panel-switch"}>
          <span id={"fixed-panel-switch-lbl"}>Fixed insights</span>
          <label className={"switch"}>
            <input type={"checkbox"} onChange={props.toggle_insight_panel} />
              <span class="slider round"></span>
          </label>
        </div>
      </div>
    );
}

export default ControlPanel;
