import React from "react";
import { connect } from "react-redux";
import PropTypes from 'prop-types';
import Editor from './Editor';
import ControlPanel from './ControlPanel';

import { sessionActions } from '../actions';

class SessionContainer extends React.Component {
	static propTypes = {
	};
    constructor() {
        super();
		this.state = {
			source_in_opt: 'C',
			source_out_opt: 'X86',
			optimization_opt: 'O0',
			program_opt: 'HelloWorld'
		}
		this.updateSourceIn = this.updateSourceIn.bind(this);
		this.updateSourceOut = this.updateSourceOut.bind(this);
		this.updateOptimization = this.updateOptimization.bind(this);
		this.updateProgram = this.updateProgram.bind(this);
		this._runSession = this._runSession.bind(this);
    }

	componentDidUpdate() {
		// console.log("componentDidUpdate: ", this.state, this.props);
	}

	_runSession(action) {
		return function(a) {
			action(a, function() {
				const {
					source_in_opt,
					source_out_opt,
					optimization_opt,
					program_opt,
				} = this.state;
				this.props.dispatch(sessionActions.runSession(
					source_in_opt,
					source_out_opt,
					optimization_opt,
					program_opt
				));
			});
		}
	}

	updateSourceIn({value}, f) {
		this.setState({
			source_in_opt: value
		}, f)
	}
	updateSourceOut({value}, f) {
		this.setState({
			source_out_opt: value
		}, f)
	}
	updateOptimization({value}, f) {
		this.setState({
			optimization_opt: value
		}, f)
	}
	updateProgram({value}, f) {
		this.setState({
			program_opt: value
		}, f)
	}

    render() {
		const {sessionState} = this.props;
		const {
			source_in_opt,
			source_out_opt,
			optimization_opt,
			program_opt,
		} = this.state;
		return (
			<div id='editor-area-container'>
				<ControlPanel
					source_in={source_in_opt}
					source_out={source_out_opt}
					optimization={optimization_opt}
					program_sample={program_opt}
					source_in_onChange={this._runSession(this.updateSourceIn)}
					source_out_onChange={this._runSession(this.updateSourceOut)}
					opt_onChange={this._runSession(this.updateOptimization)}
					program_sample_onChange={this._runSession(this.updateProgram)}
				/>
				<Editor
					source={sessionState?.source_in}
					target={sessionState?.source_out}
				/>
			</div>
		)
    }
}

const mapStateToProps = (state) => {
	const {sessionState} = state.runSession;
	return {
		sessionState
	}
}

export default connect(mapStateToProps, null)(SessionContainer);
