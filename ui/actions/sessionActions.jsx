import api from '../helpers/api';
import {
    RUN_SESSION,
    RUN_SESSION_FAILED,
    RUN_SESSION_START
} from './types';

export const runSession = (session_source_in, session_source_out, session_optimization, session_program) => {
    return dispatch => {
        dispatch({type:RUN_SESSION_START});
        const reqBody = {session_source_in, session_source_out, session_optimization, session_program};
        api.post('/session', reqBody)
        .then((response) => {
            if (response.ok) {
                console.log(response.data);
                dispatch({type:RUN_SESSION, data: response.data});
            } else {
                let err = response.status === 404 ? 'not found' : response.problem;
                dispatch({type:RUN_SESSION_FAILED, error: err});
            }
        })
    }
}
